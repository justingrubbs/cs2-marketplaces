module CS2.Marketplaces.CSFloat.RateLimit (
    WithHeaders,
    CSFloatLimiterPolicy (..),
    defaultCSFloatLimiterPolicy,
    CSFloatBucket (..),
    newCSFloatLimiter,
    toll,
    reconcileFromHeaders,
    cacheHit,
    module CS2.RateLimit,
)
where

import CS2.RateLimit
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Servant.API
import Text.Read (readMaybe)

type WithHeaders =
    Headers
        '[ Header "x-ratelimit-limit" Text
         , Header "x-ratelimit-remaining" Text
         , Header "x-ratelimit-reset" Text
         , Header "cf-cache-status" Text
         ]

data CSFloatLimiterPolicy = CSFloatLimiterPolicy {capacityRatio :: Double}

defaultCSFloatLimiterPolicy :: CSFloatLimiterPolicy
defaultCSFloatLimiterPolicy = CSFloatLimiterPolicy 0.1

data CSFloatBucket
    = Global
    | GetListings
    | GetInventory
    | GetTradeHistory
    | GetSales
    | GetSimilarListings
    | GetPriceHistory
    | UpdateBulkListings
    deriving (Eq, Ord, Enum, Bounded, Show)

newCSFloatLimiter :: IO (Limiter CSFloatBucket)
newCSFloatLimiter =
    newLimiter
        =<< maybe (fail "csfloatLimiterSpec: non-positive/non-finite capacity or rate") pure csfloatLimiterSpec

csfloatLimiterSpec :: Maybe [(CSFloatBucket, Capacity, Rate)]
csfloatLimiterSpec =
    traverse
        (\(k, mc, mr) -> (,,) k <$> mc <*> mr)
        [ (Global, mkCapacity 1000, mkRate (50000 / day))
        , (GetListings, mkCapacity 10, mkRate (200 / hour))
        , (GetInventory, mkCapacity 6, mkRate (120 / day))
        , (GetTradeHistory, mkCapacity 5, mkRate (100 / (10 * minute)))
        , (GetSales, mkCapacity 25, mkRate (500 / day))
        , (GetSimilarListings, mkCapacity 1, mkRate (20 / minute))
        , (GetPriceHistory, mkCapacity 25, mkRate (500 / day))
        , (UpdateBulkListings, mkCapacity 1, mkRate (10 / minute))
        ]
  where
    day, hour, minute :: Double
    day = 24 * hour
    hour = 60 * minute
    minute = 60

toll :: CSFloatBucket -> Double -> Toll CSFloatBucket
toll b n = fromMaybe (error "CSFloat.RateLimit.toll: non-positive cost") (mkToll b n)

{-  `reconcileFromHeaders` re-derives `capacity` and `rate` from a response's headers.

    `capacity` is set to the minimum of `limit * policy.capacityRatio` and `remaining`,
    as a `capacity > remaining` will result in a 429.

    `rate` falls as `remaining` decreases, but rises as `reset` approaches.

    Unfortunately, CSFloat headers report only on one bucket, so the caller must
    specify which bucket the reconciliation will apply to.
-}
reconcileFromHeaders :: Limiter CSFloatBucket -> CSFloatLimiterPolicy -> CSFloatBucket -> WithHeaders a -> IO ()
reconcileFromHeaders lim policy bucket resp =
    case (readHdr limitH, readHdr remainingH, readHdr resetH) of
        (Just limit, Just remaining, Just reset) -> do
            now <- realToFrac <$> getPOSIXTime
            let timeLeft = max 60 (reset - now) -- problematic when the period length is only a minute, as the rate only ever slows.
                rate = remaining / timeLeft * 0.9
                capacity = min remaining (limit * policy.capacityRatio)
            reconcile lim bucket (mkRate rate) (mkCapacity capacity)
        _ -> pure ()
  where
    limitH = lookupResponseHeader @"x-ratelimit-limit" @Text resp
    remainingH = lookupResponseHeader @"x-ratelimit-remaining" @Text resp
    resetH = lookupResponseHeader @"x-ratelimit-reset" @Text resp

    readHdr :: ResponseHeader h Text -> Maybe Double
    readHdr (Header t) = readMaybe (T.unpack t)
    readHdr _ = Nothing

cacheHit :: WithHeaders a -> Bool
cacheHit resp = case lookupResponseHeader @"cf-cache-status" @Text resp of
    Header t -> T.toUpper t == "HIT"
    _ -> False
