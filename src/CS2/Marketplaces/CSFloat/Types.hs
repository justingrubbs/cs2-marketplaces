module CS2.Marketplaces.CSFloat.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API

data CSFloatSortBy
    = LowestPrice
    | HighestPrice
    | MostRecent
    | ExpiresSoon
    | LowestFloat
    | HighestFloat
    | BestDeal
    | HighestDiscount
    | FloatRank
    | NumBids
    deriving (Show)

instance ToHttpApiData CSFloatSortBy where
    toQueryParam = \case
        LowestPrice -> "lowest_price"
        HighestPrice -> "highest_price"
        MostRecent -> "most_recent"
        ExpiresSoon -> "expires_soon"
        LowestFloat -> "lowest_float"
        HighestFloat -> "highest_float"
        BestDeal -> "best_deal"
        HighestDiscount -> "highest_discount"
        FloatRank -> "float_rank"
        NumBids -> "num_bids"

data CSFloatCategory
    = Any
    | Normal
    | StatTrak
    | Souvenir
    deriving (Show)

instance ToHttpApiData CSFloatCategory where
    toQueryParam t =
        let val :: Int = case t of
                Any -> 0
                Normal -> 1
                StatTrak -> 2
                Souvenir -> 3
         in T.show val

data CSFloatType
    = BuyNow
    | Auction
    deriving (Show)

instance FromJSON CSFloatType where
    parseJSON = withText "CSFloatType" $ \case
        "buy_now" -> pure BuyNow
        "auction" -> pure Auction
        other -> fail $ "Unknown CSFloatType: " <> T.unpack other

instance ToHttpApiData CSFloatType where
    toQueryParam = \case
        BuyNow -> "buy_now"
        Auction -> "auction"

data GetListingsResponse = GetListingsResponse
    { data_ :: [CSFloatListing]
    , cursor :: Text
    }
    deriving (Show, Generic)

instance FromJSON GetListingsResponse where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "data_" -> "data"
                    other -> other
                }

data CSFloatListing = CSFloatListing
    { id :: Text
    , created_at :: UTCTime
    , type_ :: CSFloatType
    , price :: Int
    , state :: CSFloatState
    , seller :: CSFloatSeller
    , reference :: CSFloatReference
    , item :: CSFloatItem
    }
    deriving (Show, Generic)

instance FromJSON CSFloatListing where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }

data CSFloatState
    = Listed
    | Other Text
    deriving (Show, Generic)

instance FromJSON CSFloatState where
    parseJSON = withText "CSFloatState" $ \case
        "listed" -> pure Listed
        other -> pure $ Other other

data CSFloatSeller = CSFloatSeller
    { away :: Bool
    , flags :: Int
    , obfuscated_id :: Text
    , online :: Bool
    , stall_public :: Bool
    , statistics :: CSFloatSellerStatistics
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatSellerStatistics = CSFloatSellerStatistics
    { median_trade_time :: Int
    , total_avoided_trades :: Int
    , total_failed_trades :: Int
    , total_trades :: Int
    , total_verified_trades :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatReference = CSFloatReference
    { base_price :: Int
    , float_factor :: Double
    , predicted_price :: Int
    , keychain_price :: Int
    , quantity :: Int
    , last_updated :: UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatItem = CSFloatItem
    { asset_id :: Text
    , def_index :: Int
    , paint_index :: Int
    , paint_seed :: Int
    , float_value :: Double
    , icon_url :: Text
    , is_stattrak :: Bool
    , is_souvenir :: Bool
    , rarity :: Int
    , quality :: Int
    , market_hash_name :: Text
    , -- , stickers :: [CSFloatSticker]
      -- , keychains :: [CSFloatKeychain]
      low_rank :: Int
    , high_rank :: Int
    , tradable :: Int
    , inspect_link :: Text
    , cs2_screenshot_id :: Text
    , cs2_screenshot_at :: UTCTime
    , is_commodity :: Bool
    , type_ :: Text
    , rarity_name :: Text
    , type_name :: Text
    , item_name :: Text
    , wear_name :: Text
    , description :: Text
    , collection :: Text
    , -- , badges :: [CSFloatBadge]
      serialized_inspect :: Text
    , gs_sig :: Text
    }
    deriving (Show, Generic)

instance FromJSON CSFloatItem where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }
