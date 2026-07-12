module Main where

import CS2.Marketplaces.CSFloat
import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import qualified Data.List as L
import qualified Data.Text as T
import Servant.Client (ClientError)
import System.Environment (getEnv)

{- | REPL for interacting with CS2.Marketplaces.CSFloat as a typical library consumer.
  For our use case, this is useful for sifting through API responses and in ensuring the library
  module is self-sufficient.
-}
main :: IO ()
main = putStrLn "CSFloat Repl:"

loadCfg :: IO CSFloatConfig
loadCfg = do
    loadFile defaultConfig{configPath = ["config/.env"]}
    key <- getEnv "CSFLOAT_API_KEY"
    mkCSFloatConfig (T.pack key)

-- Should not be ClientError; servant should be not relevant to consumer
testGetListings :: IO (Either ClientError GetListingsResponse)
testGetListings = do
    cfg <- loadCfg
    getListings' cfg defaultGetListingsQuery

testGetListing :: IO (Either ClientError Listing)
testGetListing = do
    cfg <- loadCfg
    getListing' cfg 896429455789131961

testPostBuyNowListing :: IO (Either ClientError Listing)
testPostBuyNowListing = do
    cfg <- loadCfg
    let plr =
            PostBuyNowListing
                { asset_id = "30744610817"
                , price = 100000
                , max_offer_discount = Nothing
                , description = Just "Test posting buy now listing"
                , private = True
                }
    postListing' cfg plr

testPostAuctionListing :: IO (Either ClientError Listing)
testPostAuctionListing = do
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
    postListing' cfg plr

testUpdateListingPrice :: IO (Either ClientError Listing)
testUpdateListingPrice = do
    cfg <- loadCfg
    updateListingPrice' cfg "997340244347060303" UpdateListingPriceRequest{price = 100101}

testDeleteListing :: IO (Either ClientError ())
testDeleteListing = do
    cfg <- loadCfg
    postResult <- testPostBuyNowListing
    either (pure . Left) (deleteListing' cfg . (.id)) postResult

testGetMe :: IO (Either ClientError Me)
testGetMe = do
    cfg <- loadCfg
    getMe' cfg

testGetInventory :: IO (Either ClientError [Item])
testGetInventory = do
    cfg <- loadCfg
    getInventory' cfg

-- Fix cursor
testGetStall :: IO (Either ClientError Stall)
testGetStall = do
    cfg <- loadCfg
    getStall' cfg "76561198203252187" (Just "CAEQBxjaxdvSBiC5gYik7Y_A6w0ogZCWxHI46IsGivFypJ-W") Nothing

testGetSales :: IO (Either ClientError [Listing])
testGetSales = do
    cfg <- loadCfg
    getSales' cfg "AK-47 | Redline (Minimal Wear)" Nothing

testGetTransactions :: IO (Either ClientError Transactions)
testGetTransactions = do
    cfg <- loadCfg
    getTransactions' cfg (Just 1) (Just 2)

testGetWatchlist :: IO (Either ClientError Watchlist)
testGetWatchlist = do
    cfg <- loadCfg
    getWatchlist' cfg (Just 50)

testDeleteWatchlist :: IO (Either ClientError ())
testDeleteWatchlist = do 
    cfg <- loadCfg
    deleteWatchlist' cfg 997340244347060303

testGetTradeHistory :: IO (Either ClientError Trades)
testGetTradeHistory = do
    cfg <- loadCfg
    getTradeHistory' cfg (Just "buyer") Nothing (Just 5)
