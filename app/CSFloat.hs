module Main where

import CS2.Marketplaces.CSFloat
import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import qualified Data.List as L
import Data.Maybe (isJust)
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
    getListings' cfg defaultListingsQuery

tempTest = do
    elistingsresponse <- testGetListings
    case elistingsresponse of
        Left ce -> pure $ Left ce
        Right listingsresponse -> do
            let listings = listingsresponse.data_
                checkField = keychains . item
                temp = map checkField $ L.filter (isJust . checkField) listings
            pure $ Right $ temp
