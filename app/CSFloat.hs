{- | REPL for interacting with CS2.Marketplaces.CSFloat as a typical library consumer.

Layout:

  1. Session config      - loading \/ caching a 'CSFloatConfig' for the session
  2. Test data           - defines data used in all @try*@ and @inspect*@
  3. Per-endpoint calls   - @try*@: one thin wrapper per library endpoint
  4. Raw header inspection - @inspect*@: bypass the library, dump response headers
  5. Pagination & scraping - walk paginated endpoints, collect raw items
  6. Response analysis     - summarise scraped items (enumerate variants, etc.)
-}
module Main where

import CS2.Marketplaces.CSFloat
import CS2.Marketplaces.CSFloat.RateLimit (peekLive)
import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import Control.Concurrent
import Control.Monad (void)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = putStrLn "CSFloat Repl:"

-- ---------------------------------------------------------------------------
-- 1. Session config
-- ---------------------------------------------------------------------------

{-# NOINLINE cfgRef #-}
cfgRef :: IORef (Maybe CSFloatConfig)
cfgRef = unsafePerformIO (newIORef Nothing)

-- | Build a fresh config (fresh manager + fresh limiter) from @config\/.env@.
newCfg :: IO CSFloatConfig
newCfg = do
    loadFile defaultConfig{configPath = ["config/.env"]}
    key <- getEnv "CSFLOAT_API_KEY"
    mkCSFloatConfig (T.pack key)

{- | The session config: built once by the first caller, cached in 'cfgRef'
after that so the limiter state persists across calls.
-}
getCfg :: IO CSFloatConfig
getCfg =
    readIORef cfgRef >>= \case
        Just cfg -> pure cfg
        Nothing -> do
            cfg <- newCfg
            writeIORef cfgRef (Just cfg)
            pure cfg

-- | Snapshot whatever limiter the cached config is holding.
peekLimiter :: IO ()
peekLimiter = getCfg >>= peekLive . limiter >>= mapM_ print

-- | Repeat @action@ up to @n@ times, printing each result; stop early on 'Left'.
run :: (Show a) => Int -> IO (Either a b) -> IO ()
run n action = go n
  where
    go k
        | k <= 0 = pure ()
        | otherwise = do
            result <- action
            print (void result)
            either (const (pure ())) (const (go (k - 1))) result

-- ---------------------------------------------------------------------------
-- 2. Test data
-- ---------------------------------------------------------------------------

testListing :: Text
testListing = "1014773761330774474"

testListings :: [Text]
testListings = ["1014773761423049163", "1014773761330774474"]

testPostBuyNowListing, testPostAuctionListing :: PostListingRequest
testPostBuyNowListing =
    PostBuyNowListing
        { asset_id = "30744610817"
        , price = 100000
        , max_offer_discount = Nothing
        , description = Just "Buy Now - MP7"
        , private = True
        }
testPostAuctionListing =
    PostAuctionListing
        { asset_id = "24609966769"
        , price = 100000
        , max_offer_discount = Nothing
        , reserve_price = 100000
        , duration_days = 3
        , description = Just "Auction - Famas"
        , private = True
        }

testPostBulkListingsRequest :: PostBulkListingsRequest
testPostBulkListingsRequest =
    PostBulkListingsRequest
        { items =
            [ PostBuyNowListing
                { asset_id = "30744610817"
                , price = 100000
                , max_offer_discount = Nothing
                , description = Just "Buy Now - MP7"
                , private = True
                }
            , PostBuyNowListing
                { asset_id = "24609966769"
                , price = 100000
                , max_offer_discount = Nothing
                , description = Just "Buy Now - MP7"
                , private = True
                }
            ]
        , notary_payload = Nothing
        }

testUpdateListingRequest :: UpdateListingRequest
testUpdateListingRequest =
    UpdateListingRequest
        { price = Just 100101
        , description = Just "Updated description"
        , private = Just True
        , max_offer_discount = Nothing
        , use_default_offer_discount = Nothing
        }

testUpdateBulkListingsRequest :: UpdateBulkListingsRequest
testUpdateBulkListingsRequest =
    UpdateBulkListingsRequest
        { modifications =
            map
                ( \contractId ->
                    ListingModification
                        { contract_id = contractId
                        , price = Just 100101
                        , description = Just "updateBulkListings"
                        , private = Just True
                        , max_offer_discount = Nothing
                        , use_default_offer_discount = Nothing
                        }
                )
                testListings
        }

testDeleteBulkListingsRequest :: DeleteBulkListingsRequest
testDeleteBulkListingsRequest = DeleteBulkListingsRequest testListings

-- ---------------------------------------------------------------------------
-- 3. Per-endpoint calls
-- ---------------------------------------------------------------------------

tryGetListings :: IO (Either CSFloatError GetListingsResponse)
tryGetListings = getCfg >>= \cfg -> getListings cfg defaultGetListingsQuery

tryGetListing :: IO (Either CSFloatError Listing)
tryGetListing = getCfg >>= \cfg -> getListing cfg testListing

tryPostBuyNowListing :: IO (Either CSFloatError Listing)
tryPostBuyNowListing = do
    cfg <- getCfg
    let plr = testPostBuyNowListing
    postListing cfg plr

tryPostAuctionListing :: IO (Either CSFloatError Listing)
tryPostAuctionListing = do
    cfg <- getCfg
    let plr =
            PostAuctionListing
                { asset_id = "30744610817"
                , price = 100000
                , max_offer_discount = Nothing
                , reserve_price = 100000
                , duration_days = 3
                , description = Just "Test posting auction listing"
                , private = True
                }
    postListing cfg plr

tryDeleteListing :: IO (Either CSFloatError ())
tryDeleteListing = do
    cfg <- getCfg
    deleteListing cfg testListing

tryUpdateListing :: IO (Either CSFloatError Listing)
tryUpdateListing =
    getCfg >>= \cfg ->
        updateListing
            cfg
            testListing
            testUpdateListingRequest

tryPostBulkListings :: IO (Either CSFloatError Listings)
tryPostBulkListings = do
    cfg <- getCfg
    postBulkListings cfg testPostBulkListingsRequest

tryUpdateBulkListings :: IO (Either CSFloatError Listings)
tryUpdateBulkListings = do
    cfg <- getCfg
    updateBulkListings cfg testUpdateBulkListingsRequest

tryDeleteBulkListings :: IO (Either CSFloatError ())
tryDeleteBulkListings = do
    cfg <- getCfg
    deleteBulkListings cfg testDeleteBulkListingsRequest

tryPostWatchlist :: IO (Either CSFloatError ())
tryPostWatchlist = getCfg >>= \cfg -> postWatchlist cfg testListing

tryDeleteWatchlist :: IO (Either CSFloatError ())
tryDeleteWatchlist = getCfg >>= \cfg -> deleteWatchlist cfg testListing

tryGetSimilarListings :: IO (Either CSFloatError [Listing])
tryGetSimilarListings = do
    cfg <- getCfg
    getSimilarListings cfg testListing

tryGetMe :: IO (Either CSFloatError Me)
tryGetMe = getCfg >>= \cfg -> getMe cfg

tryGetInventory :: IO (Either CSFloatError [Item])
tryGetInventory = getCfg >>= \cfg -> getInventory cfg

tryGetTransactions :: IO (Either CSFloatError Transactions)
tryGetTransactions = getCfg >>= \cfg -> getTransactions cfg (Just 1) (Just 2)

tryGetWatchlist :: IO (Either CSFloatError Listings)
tryGetWatchlist = getCfg >>= \cfg -> getWatchlist cfg (Just 50)

tryGetTradeHistory :: IO (Either CSFloatError Trades)
tryGetTradeHistory = getCfg >>= \cfg -> getTradeHistory cfg (Just "buyer") Nothing (Just 5)

-- Fix cursor
tryGetStall :: IO (Either CSFloatError Stall)
tryGetStall =
    getCfg >>= \cfg ->
        getStall cfg "76561198203252187" (Just "CAEQBxjaxdvSBiC5gYik7Y_A6w0ogZCWxHI46IsGivFypJ-W") Nothing

tryGetSales :: IO (Either CSFloatError [Listing])
tryGetSales = getCfg >>= \cfg -> getSales cfg "AK-47 | Redline (Minimal Wear)" Nothing

tryGetPriceHistory :: IO (Either CSFloatError [HistoryPoint])
tryGetPriceHistory = getCfg >>= \cfg -> getPriceHistory cfg "AK-47 | Redline (Minimal Wear)" Nothing

-- ---------------------------------------------------------------------------
-- 4. Raw header inspection
-- ---------------------------------------------------------------------------
-- Hit the API directly (no library, no limiter) and print response headers.

inspectRequest :: (ToJSON a) => CSFloatConfig -> ByteString -> String -> Maybe a -> IO ()
inspectRequest cfg httpMethod path mBody = do
    manager <- newManager tlsManagerSettings
    req0 <- parseRequest ("https://csfloat.com" <> path)
    let req =
            req0
                { method = httpMethod
                , requestHeaders =
                    ("Authorization", encodeUtf8 (apiKey cfg))
                        : maybe [] (const [("Content-Type", "application/json")]) mBody
                , requestBody = maybe (requestBody req0) (RequestBodyLBS . encode) mBody
                }
    resp <- httpLbs req manager
    mapM_ print (responseHeaders resp)

inspectGetListings :: CSFloatConfig -> IO ()
inspectGetListings cfg = inspectRequest cfg "GET" "/api/v1/listings" (Nothing :: Maybe ())

inspectGetListing :: CSFloatConfig -> IO ()
inspectGetListing cfg = inspectRequest cfg "GET" ("/api/v1/listings/" <> T.unpack testListing) (Nothing :: Maybe ())

inspectPostListing :: CSFloatConfig -> IO ()
inspectPostListing cfg =
    inspectRequest
        cfg
        "POST"
        "/api/v1/listings"
        ( Just
            PostBuyNowListing
                { asset_id = "30744610817"
                , price = 100000
                , max_offer_discount = Nothing
                , description = Just "Header inspection — will be cancelled"
                , private = True
                }
        )

inspectDeleteListing :: CSFloatConfig -> IO ()
inspectDeleteListing cfg =
    inspectRequest cfg "DELETE" ("/api/v1/listings/" <> T.unpack testListing) (Nothing :: Maybe ())

inspectUpdateListing :: CSFloatConfig -> IO ()
inspectUpdateListing cfg =
    inspectRequest
        cfg
        "PATCH"
        ("/api/v1/listings/" <> T.unpack testListing)
        (Just testUpdateListingRequest)

inspectPostBulkListings :: CSFloatConfig -> IO ()
inspectPostBulkListings cfg =
    inspectRequest
        cfg
        "POST"
        "/api/v1/listings/bulk-list"
        (Just testPostBulkListingsRequest)

inspectUpdateBulkListings :: CSFloatConfig -> IO ()
inspectUpdateBulkListings cfg =
    inspectRequest
        cfg
        "PATCH"
        "/api/v1/listings/bulk-modify"
        (Just testUpdateBulkListingsRequest)

inspectDeleteBulkListings :: CSFloatConfig -> IO ()
inspectDeleteBulkListings cfg =
    inspectRequest
        cfg
        "PATCH"
        "/api/v1/listings/bulk-delist"
        (Just testDeleteBulkListingsRequest)

inspectPostWatchlist :: CSFloatConfig -> IO ()
inspectPostWatchlist cfg =
    inspectRequest cfg "POST" ("/api/v1/listings/" <> T.unpack testListing <> "/watchlist") (Nothing :: Maybe ())

inspectDeleteWatchlist :: CSFloatConfig -> IO ()
inspectDeleteWatchlist cfg =
    inspectRequest cfg "DELETE" ("/api/v1/listings/" <> T.unpack testListing <> "/watchlist") (Nothing :: Maybe ())

inspectGetSimilarListings :: CSFloatConfig -> IO ()
inspectGetSimilarListings cfg =
    inspectRequest
        cfg
        "GET"
        ("/api/v1/listings/" <> T.unpack testListing <> "/similar")
        (Nothing :: Maybe ())

inspectGetMe :: CSFloatConfig -> IO ()
inspectGetMe cfg = inspectRequest cfg "GET" "/api/v1/me" (Nothing :: Maybe ())

inspectGetInventory :: CSFloatConfig -> IO ()
inspectGetInventory cfg = inspectRequest cfg "GET" "/api/v1/me/inventory" (Nothing :: Maybe ())

inspectGetTransactions :: CSFloatConfig -> IO ()
inspectGetTransactions cfg = inspectRequest cfg "GET" "/api/v1/me/transactions" (Nothing :: Maybe ())

inspectGetWatchlist :: CSFloatConfig -> IO ()
inspectGetWatchlist cfg = inspectRequest cfg "GET" "/api/v1/me/watchlist" (Nothing :: Maybe ())

inspectGetTradeHistory :: CSFloatConfig -> IO ()
inspectGetTradeHistory cfg = inspectRequest cfg "GET" "/api/v1/me/trades" (Nothing :: Maybe ())

inspectGetStall :: CSFloatConfig -> IO ()
inspectGetStall cfg = inspectRequest cfg "GET" "/api/v1/users/76561198203252187/stall" (Nothing :: Maybe ())

inspectGetSales :: CSFloatConfig -> IO ()
inspectGetSales cfg =
    inspectRequest cfg "GET" "/api/v1/history/AK-47%20%7C%20Redline%20(Minimal%20Wear)/sales" (Nothing :: Maybe ())

inspectGetPriceHistory :: CSFloatConfig -> IO ()
inspectGetPriceHistory cfg =
    inspectRequest cfg "GET" "/api/v1/history/AK-47%20%7C%20Redline%20(Minimal%20Wear)/graph" (Nothing :: Maybe ())

-- ---------------------------------------------------------------------------
-- 5. Pagination & scraping
-- ---------------------------------------------------------------------------

{- | Walk up to maxPages numbered pages of a count-paginated endpoint,
collecting every raw item (no projection).
-}
fetchAllPaged ::
    -- | maxPages
    Int ->
    -- | limit (page size)
    Int ->
    (Int -> IO (Either CSFloatError resp)) ->
    (resp -> [a]) ->
    (resp -> Int) ->
    IO [a]
fetchAllPaged maxPages limit fetchPage getItems getCount = go 1 []
  where
    go page acc
        | page > maxPages = pure acc
        | otherwise = do
            result <- fetchPage page
            case result of
                Left err -> do
                    putStrLn $ "fetchAllPaged: stopping early at page " <> show page <> ": " <> show err
                    pure acc
                Right resp -> do
                    let items = getItems resp
                        acc' = acc <> items
                        total = getCount resp
                    if null items || page * limit >= total
                        then pure acc'
                        else go (page + 1) acc'

-- | Walk a cursor-paginated endpoint up to maxPages, collecting every raw item.
fetchAllCursor ::
    Int ->
    (Maybe Text -> IO (Either CSFloatError (resp, Maybe Text))) ->
    (resp -> [a]) ->
    IO [a]
fetchAllCursor maxPages fetchPage getItems = go 0 Nothing []
  where
    go pageCount mCursor acc
        | pageCount >= maxPages = pure acc
        | otherwise = do
            result <- fetchPage mCursor
            case result of
                Left err -> do
                    putStrLn $ "fetchAllCursor: stopping early after " <> show pageCount <> " pages: " <> show err
                    pure acc
                Right (resp, nextCursor) -> do
                    let acc' = acc <> getItems resp
                    case nextCursor of
                        Just c -> go (pageCount + 1) (Just c) acc'
                        Nothing -> pure acc'

forkFetch :: IO a -> IO (ThreadId, IORef (Maybe a))
forkFetch f = do
    _ <- getCfg
    out <- newIORef Nothing
    tid <- forkIO (f >>= writeIORef out . Just)
    pure (tid, out)

fetchAllListings :: IO [Listing]
fetchAllListings = do
    cfg <- getCfg
    fetchAllCursor
        10
        (\mc -> fmap (\r -> (r, r.cursor)) <$> getListings cfg defaultGetListingsQuery{lqCursor = mc})
        (.data_)

fetchAllTransactions :: IO [Transaction]
fetchAllTransactions = do
    cfg <- getCfg
    let limit = 50
    fetchAllPaged 10 limit (\page -> getTransactions cfg (Just page) (Just limit)) (.transactions) (.count)

fetchAllTrades :: IO [Trade]
fetchAllTrades = do
    cfg <- getCfg
    let limit = 50
    fetchAllPaged 10 limit (\page -> getTradeHistory cfg Nothing (Just page) (Just limit)) (.trades) (.count)

-- ---------------------------------------------------------------------------
-- 6. Response analysis
-- ---------------------------------------------------------------------------
-- Enumerate the distinct variants present in a batch of scraped items.

listingStates :: [Listing] -> S.Set ListingState
listingStates = S.fromList . map (.state)

transactionTypes :: [Transaction] -> S.Set TransactionType
transactionTypes = S.fromList . map (.type_)

transactionDetailsTypes :: [Transaction] -> S.Set (Maybe TransactionDetailsType)
transactionDetailsTypes = S.fromList . map ((.type_) . (.details))

tradeStates :: [Trade] -> S.Set TradeState
tradeStates = S.fromList . map (.state)

tradeVerificationModes :: [Trade] -> S.Set TradeVerificationMode
tradeVerificationModes = S.fromList . map (.verification_mode)

tradeSteamOfferStates :: [Trade] -> S.Set (Maybe SteamTradeOfferState)
tradeSteamOfferStates = S.fromList . map ((.state) . (.steam_offer))

tradeContractStates :: [Trade] -> S.Set ContractState
tradeContractStates = S.fromList . map ((.state) . (.contract))
