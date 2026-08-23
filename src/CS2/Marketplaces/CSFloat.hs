module CS2.Marketplaces.CSFloat (
    module CS2.Marketplaces.CSFloat.Types,
    module CS2.Marketplaces.CSFloat,
) where

import CS2.Marketplaces.CSFloat.API
import CS2.Marketplaces.CSFloat.Types
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
import Servant.Client.Generic

data CSFloatConfig = CSFloatConfig
    { apiKey :: Text
    , clientEnv :: ClientEnv
    }

csfloatClient :: CSFloatAPI (AsClientT ClientM)
csfloatClient = genericClient

mkCSFloatConfig :: Text -> IO CSFloatConfig
mkCSFloatConfig key = do
    mgr <- newManager tlsManagerSettings
    let env = mkClientEnv mgr (BaseUrl Https "csfloat.com" 443 "")
    pure $ CSFloatConfig key env

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

getListings :: CSFloatConfig -> GetListingsQuery -> IO (Either ClientError GetListingsResponse)
getListings cfg GetListingsQuery{..} =
    runClientM
        ( csfloatClient.listingsAPI.getListings
            (apiKey cfg)
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
        )
        (clientEnv cfg)

-- int vs text??
getListing :: CSFloatConfig -> Int -> IO (Either ClientError Listing)
getListing cfg listingId =
    runClientM
        (csfloatClient.listingsAPI.getListing (apiKey cfg) listingId)
        (clientEnv cfg)

postListing :: CSFloatConfig -> PostListingRequest -> IO (Either ClientError Listing)
postListing cfg plr =
    runClientM
        (csfloatClient.listingsAPI.postListing (apiKey cfg) plr)
        (clientEnv cfg)

deleteListing :: CSFloatConfig -> Text -> IO (Either ClientError ())
deleteListing cfg listingId =
    runClientM
        ( csfloatClient.listingsAPI.deleteListing
            (apiKey cfg)
            listingId
        )
        (clientEnv cfg)

updateListingPrice :: CSFloatConfig -> Text -> UpdateListingPriceRequest -> IO (Either ClientError Listing)
updateListingPrice cfg listingId req =
    runClientM
        ( csfloatClient.listingsAPI.updateListingPrice
            (apiKey cfg)
            listingId
            req
        )
        (clientEnv cfg)

getMe :: CSFloatConfig -> IO (Either ClientError Me)
getMe cfg =
    runClientM
        (csfloatClient.getMe (apiKey cfg))
        (clientEnv cfg)

getInventory :: CSFloatConfig -> IO (Either ClientError [Item])
getInventory cfg =
    runClientM
        (csfloatClient.getInventory (apiKey cfg))
        (clientEnv cfg)

getStall :: CSFloatConfig -> Text -> Maybe Text -> Maybe Int -> IO (Either ClientError Stall)
getStall cfg steamId mcursor mlimit =
    runClientM
        ( csfloatClient.getStall
            steamId
            mcursor
            mlimit
        )
        (clientEnv cfg)

getSales :: CSFloatConfig -> Text -> Maybe Int -> IO (Either ClientError [Listing])
getSales cfg marketHashName mPaintIndex =
    runClientM
        ( csfloatClient.getSales
            marketHashName
            mPaintIndex
        )
        (clientEnv cfg)

getTransactions :: CSFloatConfig -> Maybe Int -> Maybe Int -> IO (Either ClientError Transactions)
getTransactions cfg mPage mLimit =
    runClientM
        ( csfloatClient.getTransactions
            (apiKey cfg)
            mPage
            mLimit
        )
        (clientEnv cfg)

getWatchlist :: CSFloatConfig -> Maybe Int -> IO (Either ClientError Watchlist)
getWatchlist cfg mLimit =
    runClientM
        ( csfloatClient.getWatchlist
            (apiKey cfg)
            mLimit
        )
        (clientEnv cfg)

deleteWatchlist :: CSFloatConfig -> Int -> IO (Either ClientError ())
deleteWatchlist cfg listingId =
    runClientM
        ( csfloatClient.deleteWatchlist
            (apiKey cfg)
            listingId
        )
        (clientEnv cfg)

getTradeHistory :: CSFloatConfig -> Maybe Text -> Maybe Int -> Maybe Int -> IO (Either ClientError Trades)
getTradeHistory cfg mRole mPage mLimit =
    runClientM
        ( csfloatClient.getTradeHistory
            (apiKey cfg)
            mRole
            mPage
            mLimit
        )
        (clientEnv cfg)
