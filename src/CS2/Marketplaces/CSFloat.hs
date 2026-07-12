module CS2.Marketplaces.CSFloat (
    module CS2.Marketplaces.CSFloat.Types,
    CSFloatConfig,
    mkCSFloatConfig,
    -- delete
    ListingsQuery (..),
    defaultListingsQuery,
    getListings',
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

data ListingsQuery = ListingsQuery
    { lqCursor :: Maybe Text
    , lqLimit :: Maybe Int
    , lqSortBy :: Maybe CSFloatSortBy
    , lqCategory :: Maybe CSFloatCategory
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
    , lqType :: Maybe CSFloatType
    , lqStickers :: Maybe Text
    }

defaultListingsQuery :: ListingsQuery
defaultListingsQuery =
    ListingsQuery
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

getListings' :: CSFloatConfig -> ListingsQuery -> IO (Either ClientError GetListingsResponse)
getListings' cfg ListingsQuery{..} =
    runClientM
        ( csfloatClient.getListings
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
