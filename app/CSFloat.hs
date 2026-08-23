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

-- Should not be ClientError; servant should be not relevant to consumer
tryGetListings :: IO (Either ClientError GetListingsResponse)
tryGetListings = getCfg >>= \cfg -> getListings cfg defaultGetListingsQuery

tryGetListing :: IO (Either ClientError Listing)
tryGetListing = getCfg >>= \cfg -> getListing cfg 896429455789131961

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
        updateListingPrice cfg "997340244347060303" UpdateListingPriceRequest{price = 100101}

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
tryDeleteWatchlist = getCfg >>= \cfg -> deleteWatchlist cfg 997340244347060303

tryGetTradeHistory :: IO (Either ClientError Trades)
tryGetTradeHistory = getCfg >>= \cfg -> getTradeHistory cfg (Just "buyer") Nothing (Just 5)

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
