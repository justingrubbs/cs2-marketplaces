module CS2.RateLimit (
    -- Buckets
    Bucket (..),
    Time,
    Rate,
    mkRate,
    getRate,
    Capacity,
    mkCapacity,
    getCapacity,
    -- Limiter
    Limiter,
    newLimiter,
    -- Tolls (constructor hidden; use 'mkToll')
    Toll,
    key,
    cost,
    mkToll,
    -- Acquiring
    AcquireResult (..),
    UnsatisfiableToll (..),
    tryAcquireSTM,
    tryAcquire,
    acquire,
    withRateLimit,
    -- Reconciliation
    reconcile,
    refundTolls,
    peekLive,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Time (NominalDiffTime)
import GHC.Clock (getMonotonicTime)
import System.Random (randomRIO)

-- | Monotonic-clock seconds (see 'getMonotonicTime'); used for all bucket timing.
type Time = Double

{- | A bucket's maximum token holding. Guaranteed positive and finite, so the
'refillBucket' clamp always bounds the bucket.
-}
newtype Capacity = Capacity Double
    deriving stock (Show, Eq)

mkCapacity :: Double -> Maybe Capacity
mkCapacity c
    | c > 0 && not (isInfinite c) = Just (Capacity c)
    | otherwise = Nothing

getCapacity :: Capacity -> Double
getCapacity (Capacity c) = c

{- | A bucket's refill rate, in tokens per second. Guaranteed positive and
finite, so 'acquire' always makes progress toward satisfying a toll.
-}
newtype Rate = Rate Double
    deriving stock (Show, Eq)

mkRate :: Double -> Maybe Rate
mkRate r
    | r > 0 && not (isInfinite r) = Just (Rate r)
    | otherwise = Nothing

getRate :: Rate -> Double
getRate (Rate r) = r

data Bucket = Bucket
    { capacity :: !Capacity
    , rate :: !Rate
    , tokens :: !Double
    , seenAt :: !Time
    }
    deriving stock (Show)

newBucket :: Capacity -> Rate -> Time -> Bucket
newBucket capacity rate seenAt =
    Bucket
        { capacity = capacity
        , rate = rate
        , tokens = getCapacity capacity
        , seenAt = seenAt
        }

refillBucket :: Time -> Bucket -> Bucket
refillBucket now b
    | elapsed <= 0 = b
    | otherwise =
        b
            { tokens = min (getCapacity (capacity b)) (tokens b + elapsed * getRate (rate b))
            , seenAt = now
            }
  where
    elapsed = now - seenAt b

newtype Limiter k = Limiter (TVar (M.Map k Bucket))

newLimiter :: (Ord k) => [(k, Capacity, Rate)] -> IO (Limiter k)
newLimiter specs = do
    now <- getMonotonicTime
    Limiter
        <$> newTVarIO (M.fromList [(k, newBucket cap r now) | (k, cap, r) <- specs])

data Toll k = Toll
    { key :: k
    , cost :: Double
    }

-- Rejects a non-positive or non-finite toll cost.
mkToll :: k -> Double -> Maybe (Toll k)
mkToll key cost
    | cost > 0 && not (isInfinite cost) = Just (Toll key cost)
    | otherwise = Nothing

data AcquireResult
    = Acquired
    | Wait !NominalDiffTime
    | Unsatisfiable
    deriving stock (Eq, Show)

data TollFailure
    = AffordIn !NominalDiffTime
    | NeverAfford
    deriving stock (Eq)

tryAcquireSTM :: (Ord k) => Limiter k -> [Toll k] -> Time -> STM AcquireResult
tryAcquireSTM (Limiter ref) tolls now = do
    m <- readTVar ref
    let unifiedTolls = unifyTolls tolls
        filledBuckets = mapMaybe (fillBucket m) unifiedTolls
        tollFailures = mapMaybe checkToll filledBuckets
    case tollFailures of
        [] -> do
            if null filledBuckets then pure () else writeTVar ref (foldl' applyToll m filledBuckets)
            pure Acquired
        _
            | elem NeverAfford tollFailures -> pure Unsatisfiable
            | otherwise -> pure . Wait $ maximum [secs | AffordIn secs <- tollFailures]
  where
    -- Unifies a list of tolls, combining repeated keys by summing their costs.
    unifyTolls :: (Ord k) => [Toll k] -> [Toll k]
    unifyTolls = map (uncurry Toll) . M.toList . M.fromListWith (+) . map (\t -> (t.key, t.cost))

    -- Check if a bucket for a given toll exists; returning the (toll, bucket) if so.
    fillBucket :: (Ord k) => M.Map k Bucket -> Toll k -> Maybe (Toll k, Bucket)
    fillBucket ctx toll = (,) toll . refillBucket now <$> M.lookup toll.key ctx

    -- Checks if a bucket can afford its toll; returning a `TollFailure` if not.
    checkToll :: (Toll k, Bucket) -> Maybe TollFailure
    checkToll (toll, bucket)
        | toll.cost > getCapacity bucket.capacity = pure NeverAfford
        | bucket.tokens >= toll.cost = Nothing
        | otherwise = pure . AffordIn $ realToFrac ((toll.cost - bucket.tokens) / getRate bucket.rate)

    -- Apply a toll to its bucket; returning the updated context.
    applyToll :: (Ord k) => M.Map k Bucket -> (Toll k, Bucket) -> M.Map k Bucket
    applyToll ctx (toll, bucket) = M.insert toll.key bucket{tokens = bucket.tokens - toll.cost} ctx

-- Try to acquire N tokens; returning whether the operation succeeded.
tryAcquire :: (Ord k) => Limiter k -> [Toll k] -> IO Bool
tryAcquire lim tolls = do
    now <- getMonotonicTime
    atomically $ (== Acquired) <$> tryAcquireSTM lim tolls now

newtype UnsatisfiableToll = UnsatisfiableToll String
    deriving stock (Show)

instance Exception UnsatisfiableToll

-- (techdebt?): Apply a limit; x amount of time before giving up?
-- (techdebt): If endpoints ever consume more than 1 token, need a
--  starvation clause to prevent larger tolls from being sidelined.
-- Acquires N tokens; throttling on failure for (wait + jitter) before retrying.
acquire :: (Ord k) => Limiter k -> [Toll k] -> IO ()
acquire lim tolls = go
  where
    go = do
        now <- getMonotonicTime
        atomically (tryAcquireSTM lim tolls now) >>= \case
            Acquired -> pure ()
            Unsatisfiable -> throwIO . UnsatisfiableToll $ "CS2.RateLimit.acquire: toll cost exceeds bucket capacity"
            Wait eta -> do
                let capMicros = 10000000 :: Integer
                    wantMicros = ceiling (eta * 1e6) + 2000 :: Integer
                    baseMicros = fromInteger (max 1000 (min capMicros wantMicros)) :: Int
                jitterMicros <- randomRIO (0, baseMicros `div` 5) -- up to +20% jitter
                threadDelay (baseMicros + jitterMicros)
                go

withRateLimit :: (Ord k) => Limiter k -> [Toll k] -> IO a -> IO a
withRateLimit lim tolls act = acquire lim tolls *> act

reconcile :: (Ord k) => Limiter k -> k -> Maybe Rate -> Maybe Capacity -> IO ()
reconcile (Limiter ref) key mRate mCap = do
    now <- getMonotonicTime
    atomically . modifyTVar' ref $
        M.adjust
            ( \b ->
                let b1 = refillBucket now b
                    b2 = maybe b1 (\r -> b1{rate = r}) mRate
                 in maybe b2 (\c -> b2{capacity = c, tokens = min (getCapacity c) b2.tokens}) mCap
            )
            key

refundTolls :: (Ord k) => Limiter k -> [Toll k] -> IO ()
refundTolls (Limiter ref) tolls = do
    now <- getMonotonicTime
    atomically . modifyTVar' ref $ \m ->
        foldl'
            ( \acc t ->
                M.adjust
                    ( \b ->
                        let b' = refillBucket now b
                         in b'{tokens = min (getCapacity b'.capacity) (b'.tokens + t.cost)}
                    )
                    t.key
                    acc
            )
            m
            tolls

peekLive :: Limiter k -> IO [(k, Bucket)]
peekLive (Limiter ref) = do
    now <- getMonotonicTime
    m <- readTVarIO ref
    pure [(k, refillBucket now b) | (k, b) <- M.toList m]
