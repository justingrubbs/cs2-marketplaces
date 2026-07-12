module CS2.Marketplaces.CSFloat.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API

-- Response types:
-------------------------------------------------------------------------------
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

-- Data types:
-------------------------------------------------------------------------------
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

data CSFloatListing = CSFloatListing
    { id :: Text
    , created_at :: UTCTime
    , type_ :: CSFloatType
    , price :: Int
    , state :: CSFloatState
    , seller :: CSFloatSeller
    , reference :: CSFloatReference
    , item :: CSFloatItem
    , is_seller :: Bool
    , min_offer_price :: Maybe Int
    , max_offer_discount :: Maybe Int
    , is_watchlisted :: Bool
    , watchers :: Int
    , auction_details :: Maybe CSFloatAuctionDetails
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
    deriving (Show, Generic, Eq)

instance FromJSON CSFloatState where
    parseJSON = withText "CSFloatState" $ \case
        "listed" -> pure Listed
        other -> pure $ Other other

data CSFloatSeller = CSFloatSeller
    { away :: Bool
    , flags :: Int
    , obfuscated_id :: Maybe Text
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
    , float_factor :: Maybe Double
    , predicted_price :: Int
    , keychain_price :: Maybe Int
    , quantity :: Int
    , last_updated :: UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatItem = CSFloatItem
    { asset_id :: Text
    , def_index :: Int
    , paint_index :: Maybe Int
    , paint_seed :: Maybe Int
    , float_value :: Maybe Double
    , icon_url :: Text
    , is_stattrak :: Maybe Bool
    , is_souvenir :: Maybe Bool
    , rarity :: Int
    , quality :: Maybe Int
    , market_hash_name :: Text
    , stickers :: Maybe [CSFloatSticker]
    , keychains :: Maybe [CSFloatKeychain]
    , low_rank :: Maybe Int
    , high_rank :: Maybe Int
    , tradable :: Int
    , inspect_link :: Maybe Text
    , cs2_screenshot_id :: Maybe Text
    , cs2_screenshot_at :: Maybe UTCTime
    , is_commodity :: Bool
    , type_ :: Text
    , rarity_name :: Text
    , type_name :: Text
    , item_name :: Text
    , wear_name :: Maybe Text
    , description :: Maybe Text
    , collection :: Maybe Text
    , badges :: Maybe [Text]
    , -- , badges :: [CSFloatBadge]
      serialized_inspect :: Maybe Text
    , gs_sig :: Maybe Text
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

data CSFloatSticker = CSFloatSticker
    { stickerId :: Int
    , slot :: Int
    , icon_url :: Text
    , name :: Text
    , reference :: Maybe CSFloatStickerReference
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatStickerReference = CSFloatStickerReference
    { price :: Int
    , quantity :: Int
    , updated_at :: UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatKeychain = CSFloatKeychain
    { stickerId :: Int
    , slot :: Int
    , offset_x :: Double
    , offset_y :: Double
    , offset_z :: Double
    , pattern :: Maybe Int
    , icon_url :: Text
    , name :: Text
    , reference :: Maybe CSFloatKeychainReference
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatKeychainReference = CSFloatKeychainReference
    { price :: Int
    , quantity :: Int
    , updated_at :: UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data CSFloatAuctionDetails = CSFloatAuctionDetails
    { reserve_price :: Int
    , expires_at :: UTCTime
    , min_next_bid :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)
