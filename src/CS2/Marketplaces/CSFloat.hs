module CS2.Marketplaces.CSFloat (
    module CS2.Marketplaces.CSFloat.Types,
    module CS2.Marketplaces.CSFloat,
) where

import CS2.Marketplaces.CSFloat.API
import CS2.Marketplaces.CSFloat.RateLimit
import CS2.Marketplaces.CSFloat.Types
import Control.Monad (void)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Generic

data CSFloatConfig = CSFloatConfig
    { apiKey :: Text
    , clientEnv :: ClientEnv
    , limiter :: Limiter CSFloatBucket
    , policy :: CSFloatLimiterPolicy
    }

csfloatClient :: CSFloatAPI (AsClientT ClientM)
csfloatClient = genericClient

mkCSFloatConfigWith :: CSFloatLimiterPolicy -> Text -> IO CSFloatConfig
mkCSFloatConfigWith pol key = do
    mgr <- newManager tlsManagerSettings
    limiter <- newCSFloatLimiter
    pure $ CSFloatConfig key (mkClientEnv mgr (BaseUrl Https "csfloat.com" 443 "")) limiter pol

mkCSFloatConfig :: Text -> IO CSFloatConfig
mkCSFloatConfig = mkCSFloatConfigWith defaultCSFloatLimiterPolicy

data GetListingsQuery = GetListingsQuery
    { lqCursor :: Maybe Text
    , lqLimit :: Maybe Int
    , lqSortBy :: Maybe SortBy
    , lqCategory :: Maybe Category
    , lqDefIndex :: Maybe Int
    , lqMinFloat :: Maybe Double
    , lqMaxFloat :: Maybe Double
    , lqRarity :: Maybe Int
    , lqPaintSeed :: Maybe Int
    , lqPaintIndex :: Maybe Int
    , lqUserId :: Maybe Int
    , lqCollection :: Maybe Text
    , lqMinPrice :: Maybe Int
    , lqMaxPrice :: Maybe Int
    , lqMarketHashName :: Maybe Text
    , lqType :: Maybe Type
    , lqStickers :: Maybe Text
    }

callWithHeaders :: CSFloatConfig -> [Toll CSFloatBucket] -> CSFloatBucket -> ClientM (WithHeaders a) -> IO (Either ClientError a)
callWithHeaders cfg tolls bucket action = withRateLimit cfg.limiter tolls $ do
    eResponse <- runClientM action (clientEnv cfg)
    withResponse eResponse $ \response -> do
        if cacheHit response
            then refundTolls cfg.limiter tolls
            else reconcileFromHeaders cfg.limiter cfg.policy bucket response
        pure . Right $ getResponse response
  where
    withResponse :: (Applicative f) => Either a t -> (t -> f (Either a b)) -> f (Either a b)
    withResponse e k = either (pure . Left) k e

-- Default `GetListingsQuery`; override fields as needed.
defaultGetListingsQuery :: GetListingsQuery
defaultGetListingsQuery =
    GetListingsQuery
        { lqCursor = Nothing
        , lqLimit = Nothing
        , lqSortBy = Nothing
        , lqCategory = Nothing
        , lqDefIndex = Nothing
        , lqMinFloat = Nothing
        , lqMaxFloat = Nothing
        , lqRarity = Nothing
        , lqPaintSeed = Nothing
        , lqPaintIndex = Nothing
        , lqUserId = Nothing
        , lqCollection = Nothing
        , lqMinPrice = Nothing
        , lqMaxPrice = Nothing
        , lqMarketHashName = Nothing
        , lqType = Nothing
        , lqStickers = Nothing
        }

-- ListingsAPI:
-------------------------------------------------------------------------------
getListings :: CSFloatConfig -> GetListingsQuery -> IO (Either ClientError GetListingsResponse)
getListings cfg GetListingsQuery{..} =
    callWithHeaders cfg [toll GlobalBucket 1, toll GetListingsBucket 1] GetListingsBucket $
        ( csfloatClient.listingsApi
            (apiKey cfg)
        ).getListings
            lqCursor
            lqLimit
            lqSortBy
            lqCategory
            lqDefIndex
            lqMinFloat
            lqMaxFloat
            lqRarity
            lqPaintSeed
            lqPaintIndex
            lqUserId
            lqCollection
            lqMinPrice
            lqMaxPrice
            lqMarketHashName
            lqType
            lqStickers

getListing :: CSFloatConfig -> Text -> IO (Either ClientError Listing)
getListing cfg listingId =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.listingsApi (apiKey cfg)).getListing listingId

postListing :: CSFloatConfig -> PostListingRequest -> IO (Either ClientError Listing)
postListing cfg plr =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.listingsApi (apiKey cfg)).postListing plr

deleteListing :: CSFloatConfig -> Text -> IO (Either ClientError ())
deleteListing cfg listingId =
    fmap void . callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.listingsApi (apiKey cfg)).deleteListing listingId

updateListingPrice :: CSFloatConfig -> Text -> UpdateListingPriceRequest -> IO (Either ClientError Listing)
updateListingPrice cfg listingId req =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.listingsApi (apiKey cfg)).updateListingPrice listingId req

deleteWatchlist :: CSFloatConfig -> Text -> IO (Either ClientError ())
deleteWatchlist cfg listingId =
    fmap void . callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.listingsApi (apiKey cfg)).deleteWatchlist listingId

-- MeAPI:
-------------------------------------------------------------------------------
getMe :: CSFloatConfig -> IO (Either ClientError Me)
getMe cfg =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.meApi (apiKey cfg)).getMe

getInventory :: CSFloatConfig -> IO (Either ClientError [Item])
getInventory cfg =
    callWithHeaders cfg [toll GlobalBucket 1, toll GetInventoryBucket 1] GetInventoryBucket $
        (csfloatClient.meApi (apiKey cfg)).getInventory

getTransactions :: CSFloatConfig -> Maybe Int -> Maybe Int -> IO (Either ClientError Transactions)
getTransactions cfg mPage mLimit =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.meApi (apiKey cfg)).getTransactions mPage mLimit

getWatchlist :: CSFloatConfig -> Maybe Int -> IO (Either ClientError Watchlist)
getWatchlist cfg mLimit =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        (csfloatClient.meApi (apiKey cfg)).getWatchlist mLimit

getTradeHistory :: CSFloatConfig -> Maybe Text -> Maybe Int -> Maybe Int -> IO (Either ClientError Trades)
getTradeHistory cfg mRole mPage mLimit =
    callWithHeaders cfg [toll GlobalBucket 1, toll GetTradeHistoryBucket 1] GetTradeHistoryBucket $
        (csfloatClient.meApi (apiKey cfg)).getTradeHistory mRole mPage mLimit

-- UsersAPI:
-------------------------------------------------------------------------------
getStall :: CSFloatConfig -> Text -> Maybe Text -> Maybe Int -> IO (Either ClientError Stall)
getStall cfg steamId mcursor mlimit =
    callWithHeaders cfg [toll GlobalBucket 1] GlobalBucket $
        csfloatClient.usersApi.getStall
            steamId
            mcursor
            mlimit

-- HistoryAPI:
-------------------------------------------------------------------------------
getSales :: CSFloatConfig -> Text -> Maybe Int -> IO (Either ClientError [Listing])
getSales cfg marketHashName mPaintIndex =
    callWithHeaders cfg [toll GlobalBucket 1, toll GetSalesBucket 1] GetSalesBucket $
        csfloatClient.historyApi.getSales
            marketHashName
            mPaintIndex
