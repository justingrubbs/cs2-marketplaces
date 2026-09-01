module CS2.Marketplaces.CSFloat.Types (
    module CS2.Marketplaces.CSFloat.Types,
    SteamTradeOfferState (..),
)
where

import CS2.Steam.Types (SteamTradeOfferState (..))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API

-- Response types:
-------------------------------------------------------------------------------
data GetListingsResponse = GetListingsResponse
    { data_ :: [Listing]
    , cursor :: Maybe Text
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

-- Request types:
-------------------------------------------------------------------------------
data PostListingRequest
    = PostBuyNowListing
        { asset_id :: Text
        , price :: Int
        , max_offer_discount :: Maybe Int
        , description :: Maybe Text
        , private :: Bool
        }
    | PostAuctionListing
        { asset_id :: Text
        , price :: Int
        , max_offer_discount :: Maybe Int
        , reserve_price :: Int
        , duration_days :: Int
        , description :: Maybe Text
        , private :: Bool
        }
    deriving (Show, Generic)

instance ToJSON PostListingRequest where
    toJSON =
        genericToJSON
            defaultOptions
                { sumEncoding = TaggedObject{tagFieldName = "type", contentsFieldName = "contents"}
                , constructorTagModifier = \case
                    "PostBuyNowListing" -> "buy_now"
                    "PostAuctionListing" -> "auction"
                    other -> other
                }

data UpdateListingRequest = UpdateListingRequest
    { price :: Maybe Int
    , description :: Maybe Text
    , private :: Maybe Bool
    , max_offer_discount :: Maybe Int
    , use_default_offer_discount :: Maybe Bool
    }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

data PostBulkListingsRequest = PostBulkListingsRequest
    { items :: [PostListingRequest]
    , notary_payload :: Maybe Text
    }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

data UpdateBulkListingsRequest = UpdateBulkListingsRequest
    { modifications :: [ListingModification]
    }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

data ListingModification = ListingModification
    { contract_id :: Text
    , price :: Maybe Int
    , description :: Maybe Text
    , private :: Maybe Bool
    , max_offer_discount :: Maybe Int
    , use_default_offer_discount :: Maybe Bool
    }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

data DeleteBulkListingsRequest = DeleteBulkListingsRequest
    { contract_ids :: [Text]
    }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

-- Data types:
-------------------------------------------------------------------------------
data SortBy
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

instance ToHttpApiData SortBy where
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

data Category
    = Any
    | Normal
    | StatTrak
    | Souvenir
    deriving (Show)

instance ToHttpApiData Category where
    toQueryParam t =
        let val :: Int = case t of
                Any -> 0
                Normal -> 1
                StatTrak -> 2
                Souvenir -> 3
         in T.show val

data Type
    = BuyNow
    | Auction
    deriving (Show, Eq)

instance FromJSON Type where
    parseJSON = withText "Type" $ \case
        "buy_now" -> pure BuyNow
        "auction" -> pure Auction
        other -> fail $ "Unknown Type: " <> T.unpack other

instance ToHttpApiData Type where
    toQueryParam = \case
        BuyNow -> "buy_now"
        Auction -> "auction"

data Listings = Listings
    { data_ :: [Listing]
    }
    deriving (Show, Generic)

instance FromJSON Listings where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "data_" -> "data"
                    other -> other
                }

data Listing = Listing
    { id :: Text
    , created_at :: UTCTime
    , type_ :: Type
    , price :: Int
    , state :: ListingState
    , seller :: Maybe Seller
    , reference :: Reference
    , item :: Item
    , is_seller :: Bool
    , min_offer_price :: Maybe Int
    , max_offer_discount :: Maybe Int
    , is_watchlisted :: Bool
    , watchers :: Int
    , auction_details :: Maybe AuctionDetails
    }
    deriving (Show, Generic)

instance FromJSON Listing where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }

data ListingState
    = Listed
    | ListingStateOther Text
    deriving (Show, Generic, Eq, Ord)

instance FromJSON ListingState where
    parseJSON = withText "ListingState" $ \case
        "listed" -> pure Listed
        other -> pure $ ListingStateOther other

data Seller = Seller
    { away :: Bool
    , flags :: Int
    , obfuscated_id :: Maybe Text
    , online :: Bool
    , stall_public :: Bool
    , statistics :: Statistics
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Statistics = Statistics
    { median_trade_time :: Int
    , total_avoided_trades :: Int
    , total_failed_trades :: Int
    , total_trades :: Int
    , total_verified_trades :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Buyer = Buyer
    { avatar :: Text
    , away :: Bool
    , flags :: Int
    , online :: Bool
    , stall_public :: Bool
    , statistics :: Statistics
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Reference = Reference
    { base_price :: Maybe Int
    , float_factor :: Maybe Double
    , predicted_price :: Maybe Int
    , keychain_price :: Maybe Int
    , quantity :: Maybe Int
    , last_updated :: Maybe UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Item = Item
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
    , stickers :: Maybe [Sticker]
    , keychains :: Maybe [Keychain]
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
    , serialized_inspect :: Maybe Text
    , gs_sig :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON Item where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }

data Sticker = Sticker
    { stickerId :: Int
    , slot :: Int
    , icon_url :: Text
    , name :: Text
    , reference :: Maybe StickerReference
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data StickerReference = StickerReference
    { price :: Int
    , quantity :: Int
    , updated_at :: UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Keychain = Keychain
    { stickerId :: Int
    , slot :: Int
    , offset_x :: Double
    , offset_y :: Double
    , offset_z :: Double
    , pattern :: Maybe Int
    , icon_url :: Text
    , name :: Text
    , reference :: Maybe KeychainReference
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data KeychainReference = KeychainReference
    { price :: Int
    , quantity :: Int
    , updated_at :: UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data AuctionDetails = AuctionDetails
    { reserve_price :: Int
    , expires_at :: UTCTime
    , min_next_bid :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Me = Me
    { user :: User
    , pending_offers :: Int
    , actionable_trades :: Int
    , has_unread_notifications :: Bool
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data User = User
    { steam_id :: Text
    , username :: Text
    , flags :: Int
    , avatar :: Text
    , email :: Text
    , phone_number :: Text
    , balance :: Int
    , pending_balance :: Int
    , stall_public :: Bool
    , away :: Bool
    , trade_token :: Text
    , payment_accounts :: PaymentAccounts
    , statistics :: UserStatistics
    , preferences :: UserPreferences
    , know_your_customer :: Text
    , extension_setup_at :: UTCTime
    , firebase_messaging :: FirebaseMessaging
    , stripe_connect :: StripeConnect
    , notification_topic_opt_out :: Int
    , obfuscated_id :: Text
    , online :: Bool
    , fee :: Double
    , withdraw_fee :: Double
    , subscriptions :: [Text]
    , has_2fa :: Bool
    , has_api_key :: Bool
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data PaymentAccounts = PaymentAccounts
    { stripe_connect :: Text
    , stripe_customer :: Text
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data UserStatistics = UserStatistics
    { total_sales :: Int
    , total_purchases :: Int
    , median_trade_time :: Int
    , total_avoided_trades :: Int
    , total_failed_trades :: Int
    , total_verified_trades :: Int
    , total_trades :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data UserPreferences = UserPreferences
    { offers_enabled :: Bool
    , max_offer_discount :: Int
    , currency :: Text
    , language :: Text
    , localize_item_names :: Bool
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data FirebaseMessaging = FirebaseMessaging
    { platform :: Maybe Text
    , last_updated :: Maybe Text
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data StripeConnect = StripeConnect
    { payouts_enabled :: Bool
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Stall = Stall
    { data_ :: [Listing]
    , cursor :: Maybe Text
    , total_count :: Int
    , total_price :: Int
    }
    deriving (Show, Generic)

instance FromJSON Stall where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "data_" -> "data"
                    other -> other
                }

data Transactions = Transactions
    { transactions :: [Transaction]
    , count :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Transaction = Transaction
    { id :: Text
    , created_at :: UTCTime
    , user_id :: Text
    , type_ :: TransactionType
    , details :: TransactionDetails
    , balance_offset :: Int
    , pending_offset :: Int
    }
    deriving (Show, Generic)

instance FromJSON Transaction where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }

data TransactionType
    = ContractPurchased
    | ContractSold
    | Deposit
    | TradeVerified
    | TransactionTypeOther Text
    deriving (Show, Generic, Eq, Ord)

instance FromJSON TransactionType where
    parseJSON = withText "TransactionType" $ \case
        "contract_purchased" -> pure ContractPurchased
        "contract_sold" -> pure ContractSold
        "deposit" -> pure Deposit
        "trade_verified" -> pure TradeVerified
        other -> pure $ TransactionTypeOther other

data TransactionDetails = TransactionDetails
    { contract_id :: Maybe Text
    , trade_id :: Maybe Text
    , type_ :: Maybe TransactionDetailsType
    }
    deriving (Show, Generic)

instance FromJSON TransactionDetails where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }

data TransactionDetailsType
    = BuyerPing
    | TransactionDetailsTypeOther Text
    deriving (Eq, Show, Generic, Ord)

instance FromJSON TransactionDetailsType where
    parseJSON = withText "TransactionDetailsType" $ \case
        "buyer_ping" -> pure BuyerPing
        other -> pure $ TransactionDetailsTypeOther other

data Trades = Trades
    { trades :: [Trade]
    , count :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data Trade = Trade
    { id :: Text
    , created_at :: UTCTime
    , buyer_id :: Text
    , buyer :: Buyer
    , seller_id :: Text
    , seller :: Seller
    , contract_id :: Text
    , accepted_at :: UTCTime
    , state :: TradeState
    , verification_mode :: TradeVerificationMode
    , steam_offer :: SteamOffer
    , verified_at :: UTCTime
    , inventory_check_status :: Maybe Int
    , contract :: TradeContract
    , trade_url :: Text
    , trade_token :: Text
    , wait_for_cancel_ping :: Bool
    , is_settlement_period :: Bool
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data TradeState
    = Verified
    | TradeStateOther Text
    deriving (Eq, Show, Generic, Ord)

instance FromJSON TradeState where
    parseJSON = withText "TradeState" $ \case
        "verified" -> pure Verified
        other -> pure $ TradeStateOther other

data TradeVerificationMode
    = Escrow
    | Inventory
    | TradeVerificationModeOther Text
    deriving (Eq, Show, Generic, Ord)

instance FromJSON TradeVerificationMode where
    parseJSON = withText "TradeVerificationMode" $ \case
        "escrow" -> pure Escrow
        "inventory" -> pure Inventory
        other -> pure $ TradeVerificationModeOther other

data SteamOffer = SteamOffer
    { id :: Maybe Text
    , state :: Maybe SteamTradeOfferState
    , is_from_seller :: Bool
    , can_cancel_at :: Maybe UTCTime
    , sent_at :: UTCTime
    , deadline_at :: Maybe UTCTime
    , updated_at :: Maybe UTCTime
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)

data TradeContract = TradeContract
    { id :: Text
    , created_at :: UTCTime
    , type_ :: Type
    , price :: Int
    , state :: ContractState
    , seller :: Seller
    , reference :: Reference
    , item :: Item
    , is_seller :: Bool
    , is_watchlisted :: Bool
    , watchers :: Int
    , sold_at :: UTCTime
    }
    deriving (Show, Generic)

instance FromJSON TradeContract where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = \case
                    "type_" -> "type"
                    other -> other
                }

data ContractState
    = Sold
    | ContractStateOther Text
    deriving (Eq, Show, Generic, Ord)

instance FromJSON ContractState where
    parseJSON = withText "ContractState" $ \case
        "sold" -> pure Sold
        other -> pure $ ContractStateOther other

data HistoryPoint = HistoryPoint
    { day :: UTCTime
    , avg_price :: Double
    , count :: Int
    }
    deriving (Show, Generic)
    deriving anyclass (FromJSON)
