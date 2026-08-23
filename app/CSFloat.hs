module Main where

import CS2.Marketplaces.CSFloat
import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import Data.IORef
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
