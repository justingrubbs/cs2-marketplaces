module Main where

import CS2.Marketplaces.CSFloat
import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import Control.Concurrent
import Data.IORef
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Servant.Client (ClientError)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

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
inspectGetListing cfg = inspectRequest cfg "GET" "/api/v1/listings/1012218546064854906" (Nothing :: Maybe ())

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

inspectDeleteListing :: CSFloatConfig -> Text -> IO ()
inspectDeleteListing cfg listingId =
    inspectRequest cfg "DELETE" ("/api/v1/listings/" <> T.unpack listingId) (Nothing :: Maybe ())

inspectUpdateListingPrice :: CSFloatConfig -> IO ()
inspectUpdateListingPrice cfg =
    inspectRequest
        cfg
        "PATCH"
        ("/api/v1/listings/1012218546064854906")
        (Just UpdateListingPriceRequest{price = 100101})

inspectGetMe :: CSFloatConfig -> IO ()
inspectGetMe cfg = inspectRequest cfg "GET" "/api/v1/me" (Nothing :: Maybe ())

inspectGetInventory :: CSFloatConfig -> IO ()
inspectGetInventory cfg = inspectRequest cfg "GET" "/api/v1/me/inventory" (Nothing :: Maybe ())

inspectGetStall :: CSFloatConfig -> IO ()
inspectGetStall cfg = inspectRequest cfg "GET" "/api/v1/users/76561198203252187/stall" (Nothing :: Maybe ())

inspectGetSales :: CSFloatConfig -> IO ()
inspectGetSales cfg =
    inspectRequest cfg "GET" "/api/v1/history/AK-47%20%7C%20Redline%20(Minimal%20Wear)/sales" (Nothing :: Maybe ())

inspectGetTransactions :: CSFloatConfig -> IO ()
inspectGetTransactions cfg = inspectRequest cfg "GET" "/api/v1/me/transactions" (Nothing :: Maybe ())

inspectGetWatchlist :: CSFloatConfig -> IO ()
inspectGetWatchlist cfg = inspectRequest cfg "GET" "/api/v1/me/watchlist" (Nothing :: Maybe ())

inspectDeleteWatchlist :: CSFloatConfig -> IO ()
inspectDeleteWatchlist cfg =
    inspectRequest cfg "DELETE" ("/api/v1/listings/1012218546064854906/watchlist") (Nothing :: Maybe ())

inspectGetTradeHistory :: CSFloatConfig -> IO ()
inspectGetTradeHistory cfg = inspectRequest cfg "GET" "/api/v1/me/trades" (Nothing :: Maybe ())

{- | REPL for interacting with CS2.Marketplaces.CSFloat as a typical library consumer.
  For our use case, this is useful for sifting through API responses and in ensuring the library
  module is self-sufficient.
-}
main :: IO ()
main = putStrLn "CSFloat Repl:"

{-# NOINLINE cfgRef #-}
cfgRef :: IORef (Maybe CSFloatConfig)
cfgRef = unsafePerformIO (newIORef Nothing)

loadCfg :: IO CSFloatConfig
loadCfg = do
    loadFile defaultConfig{configPath = ["config/.env"]}
    key <- getEnv "CSFLOAT_API_KEY"
    mkCSFloatConfig (T.pack key)

-- | Loaded once per session, cached after that.
getCfg :: IO CSFloatConfig
getCfg =
    readIORef cfgRef >>= \case
        Just cfg -> pure cfg
        Nothing -> do
            cfg <- loadCfg
            writeIORef cfgRef (Just cfg)
            pure cfg

defaultGetListingsQuery :: GetListingsQuery
defaultGetListingsQuery =
    GetListingsQuery
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

-- Per-endpoint functions:
-------------------------------------------------------------------------------
-- Should not be ClientError; servant should be not relevant to consumer
tryGetListings :: IO (Either ClientError GetListingsResponse)
tryGetListings = getCfg >>= \cfg -> getListings cfg defaultGetListingsQuery

tryGetListing :: IO (Either ClientError Listing)
tryGetListing = do
    cfg <- loadCfg
    listing <- getListing cfg 1012218546064854906
    pure listing

tryPostBuyNowListing :: IO (Either ClientError Listing)
tryPostBuyNowListing = do
    cfg <- loadCfg
    let plr =
            PostBuyNowListing
                { asset_id = "30744610817"
                , price = 100000
                , max_offer_discount = Nothing
                , description = Just "Test posting buy now listing"
                , private = True
                }
    postListing cfg plr

tryPostAuctionListing :: IO (Either ClientError Listing)
tryPostAuctionListing = do
    cfg <- loadCfg
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

tryUpdateListingPrice :: IO (Either ClientError Listing)
tryUpdateListingPrice =
    getCfg >>= \cfg ->
        updateListingPrice cfg "1012218546064854906" UpdateListingPriceRequest{price = 100101}

tryDeleteListing :: IO (Either ClientError ())
tryDeleteListing = do
    cfg <- loadCfg
    postResult <- tryPostBuyNowListing
    either (pure . Left) (deleteListing cfg . (.id)) postResult

tryGetMe :: IO (Either ClientError Me)
tryGetMe = getCfg >>= \cfg -> getMe cfg

tryGetInventory :: IO (Either ClientError [Item])
tryGetInventory = getCfg >>= \cfg -> getInventory cfg

-- Fix cursor
tryGetStall :: IO (Either ClientError Stall)
tryGetStall =
    getCfg >>= \cfg ->
        getStall cfg "76561198203252187" (Just "CAEQBxjaxdvSBiC5gYik7Y_A6w0ogZCWxHI46IsGivFypJ-W") Nothing

tryGetSales :: IO (Either ClientError [Listing])
tryGetSales = getCfg >>= \cfg -> getSales cfg "AK-47 | Redline (Minimal Wear)" Nothing

tryGetTransactions :: IO (Either ClientError Transactions)
tryGetTransactions = getCfg >>= \cfg -> getTransactions cfg (Just 1) (Just 2)

tryGetWatchlist :: IO (Either ClientError Watchlist)
tryGetWatchlist = getCfg >>= \cfg -> getWatchlist cfg (Just 50)

tryDeleteWatchlist :: IO (Either ClientError ())
tryDeleteWatchlist = getCfg >>= \cfg -> deleteWatchlist cfg 1011820418002191672

tryGetTradeHistory :: IO (Either ClientError Trades)
tryGetTradeHistory = getCfg >>= \cfg -> getTradeHistory cfg (Just "buyer") Nothing (Just 5)

-- Scraping:
-------------------------------------------------------------------------------

{- | Walk up to maxPages numbered pages of a count-paginated endpoint,
collecting every raw item (no projection).
-}
fetchAllPaged ::
    -- | maxPages
    Int ->
    -- | delayMicros
    Int ->
    -- | limit (page size)
    Int ->
    (Int -> IO (Either ClientError resp)) ->
    (resp -> [a]) ->
    (resp -> Int) ->
    IO [a]
fetchAllPaged maxPages delayMicros limit fetchPage getItems getCount = go 1 []
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
                        else do
                            threadDelay delayMicros
                            go (page + 1) acc'

-- | Walk a cursor-paginated endpoint up to maxPages, collecting every raw item.
fetchAllCursor ::
    Int ->
    Int ->
    (Maybe Text -> IO (Either ClientError (resp, Maybe Text))) ->
    (resp -> [a]) ->
    IO [a]
fetchAllCursor maxPages delayMicros fetchPage getItems = go 0 Nothing []
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
                        Just c -> do
                            threadDelay delayMicros
                            go (pageCount + 1) (Just c) acc'
                        Nothing -> pure acc'

fetchAllListings :: IO [Listing]
fetchAllListings = do
    cfg <- getCfg
    fetchAllCursor
        10
        500000
        (\mc -> fmap (\r -> (r, r.cursor)) <$> getListings cfg defaultGetListingsQuery{lqCursor = mc})
        (.data_)

fetchAllTransactions :: IO [Transaction]
fetchAllTransactions = do
    cfg <- getCfg
    let limit = 50
    fetchAllPaged 10 500000 limit (\page -> getTransactions cfg (Just page) (Just limit)) (.transactions) (.count)

fetchAllTrades :: IO [Trade]
fetchAllTrades = do
    cfg <- getCfg
    let limit = 50
    fetchAllPaged 10 500000 limit (\page -> getTradeHistory cfg Nothing (Just page) (Just limit)) (.trades) (.count)

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
