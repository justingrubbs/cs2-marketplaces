module CS2.Marketplaces.CSFloat (
    module CS2.Marketplaces.CSFloat.Types,
    module CS2.Marketplaces.CSFloat.Error,
    module CS2.Marketplaces.CSFloat,
) where

import CS2.Marketplaces.CSFloat.API
import CS2.Marketplaces.CSFloat.Error
import CS2.Marketplaces.CSFloat.RateLimit
import CS2.Marketplaces.CSFloat.Types
import Control.Exception (handle)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
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

callWithHeaders :: CSFloatConfig -> [Toll CSFloatBucket] -> CSFloatBucket -> ClientM (WithHeaders a) -> IO (Either CSFloatError a)
callWithHeaders cfg tolls bucket action =
    handle (\(UnsatisfiableToll msg) -> pure (Left (ClientLimited (T.pack msg)))) $
        withRateLimit cfg.limiter tolls $ do
            eResponse <- runClientM action (clientEnv cfg)
            case eResponse of
                Left err -> pure (Left (fromClientError err))
                Right response -> do
                    if cacheHit response
                        then refundTolls cfg.limiter tolls
                        else reconcileFromHeaders cfg.limiter cfg.policy bucket response
                    pure (Right (getResponse response))

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
getListings :: CSFloatConfig -> GetListingsQuery -> IO (Either CSFloatError GetListingsResponse)
getListings cfg GetListingsQuery{..} =
    callWithHeaders cfg [toll Global 1, toll GetListings 1] GetListings $
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

getListing :: CSFloatConfig -> Text -> IO (Either CSFloatError Listing)
getListing cfg listingId =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).getListing listingId

postListing :: CSFloatConfig -> PostListingRequest -> IO (Either CSFloatError Listing)
postListing cfg req =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).postListing req

deleteListing :: CSFloatConfig -> Text -> IO (Either CSFloatError ())
deleteListing cfg listingId =
    fmap void . callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).deleteListing listingId

updateListing :: CSFloatConfig -> Text -> UpdateListingRequest -> IO (Either CSFloatError Listing)
updateListing cfg listingId req =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).updateListing listingId req

postBulkListings :: CSFloatConfig -> PostBulkListingsRequest -> IO (Either CSFloatError Listings)
postBulkListings cfg req =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).postBulkListings req

updateBulkListings :: CSFloatConfig -> UpdateBulkListingsRequest -> IO (Either CSFloatError Listings)
updateBulkListings cfg req =
    callWithHeaders cfg [toll Global 1, toll UpdateBulkListings 1] UpdateBulkListings $
        (csfloatClient.listingsApi (apiKey cfg)).updateBulkListings req

deleteBulkListings :: CSFloatConfig -> DeleteBulkListingsRequest -> IO (Either CSFloatError ())
deleteBulkListings cfg req =
    fmap void . callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).deleteBulkListings req

postWatchlist :: CSFloatConfig -> Text -> IO (Either CSFloatError ())
postWatchlist cfg listingId =
    fmap void . callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).postWatchlist listingId

deleteWatchlist :: CSFloatConfig -> Text -> IO (Either CSFloatError ())
deleteWatchlist cfg listingId =
    fmap void . callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.listingsApi (apiKey cfg)).deleteWatchlist listingId

getSimilarListings :: CSFloatConfig -> Text -> IO (Either CSFloatError [Listing])
getSimilarListings cfg listingId =
    callWithHeaders cfg [toll Global 1, toll GetSimilarListings 1] GetSimilarListings $
        (csfloatClient.listingsApi (apiKey cfg)).getSimilarListings listingId

-- MeAPI:
-------------------------------------------------------------------------------
getMe :: CSFloatConfig -> IO (Either CSFloatError Me)
getMe cfg =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.meApi (apiKey cfg)).getMe

getInventory :: CSFloatConfig -> IO (Either CSFloatError [Item])
getInventory cfg =
    callWithHeaders cfg [toll Global 1, toll GetInventory 1] GetInventory $
        (csfloatClient.meApi (apiKey cfg)).getInventory

getTransactions :: CSFloatConfig -> Maybe Int -> Maybe Int -> IO (Either CSFloatError Transactions)
getTransactions cfg mPage mLimit =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.meApi (apiKey cfg)).getTransactions mPage mLimit

getWatchlist :: CSFloatConfig -> Maybe Int -> IO (Either CSFloatError Listings)
getWatchlist cfg mLimit =
    callWithHeaders cfg [toll Global 1] Global $
        (csfloatClient.meApi (apiKey cfg)).getWatchlist mLimit

getTradeHistory :: CSFloatConfig -> Maybe Text -> Maybe Int -> Maybe Int -> IO (Either CSFloatError Trades)
getTradeHistory cfg mRole mPage mLimit =
    callWithHeaders cfg [toll Global 1, toll GetTradeHistory 1] GetTradeHistory $
        (csfloatClient.meApi (apiKey cfg)).getTradeHistory mRole mPage mLimit

-- UsersAPI:
-------------------------------------------------------------------------------
getStall :: CSFloatConfig -> Text -> Maybe Text -> Maybe Int -> IO (Either CSFloatError Stall)
getStall cfg steamId mcursor mlimit =
    callWithHeaders cfg [toll Global 1] Global $
        csfloatClient.usersApi.getStall
            steamId
            mcursor
            mlimit

-- HistoryAPI:
-------------------------------------------------------------------------------
getSales :: CSFloatConfig -> Text -> Maybe Int -> IO (Either CSFloatError [Listing])
getSales cfg marketHashName mPaintIndex =
    callWithHeaders cfg [toll Global 1, toll GetSales 1] GetSales $
        csfloatClient.historyApi.getSales
            marketHashName
            mPaintIndex

getPriceHistory :: CSFloatConfig -> Text -> Maybe Int -> IO (Either CSFloatError [HistoryPoint])
getPriceHistory cfg marketHashName mPaintIndex =
    callWithHeaders cfg [toll Global 1, toll GetPriceHistory 1] GetPriceHistory $
        csfloatClient.historyApi.getPriceHistory
            marketHashName
            mPaintIndex
