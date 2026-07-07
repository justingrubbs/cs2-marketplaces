module CS2.Marketplaces.CSFloat where

import CS2.Marketplaces.CSFloat.Types
import Data.Text
import GHC.Generics (Generic)
import Servant.API

data CSFloatConfig = CSFloatConfig {apiKey :: Text}

data CSFloatAPI mode = CSFloatAPI
    { getListings ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "listings"
                :> QueryParam "cursor" Text
                :> QueryParam "limit" Int
                :> QueryParam "sort_by" CSFloatSortBy
                :> QueryParam "category" CSFloatCategory
                :> QueryParam "def_index" Int
                :> QueryParam "min_float" Double
                :> QueryParam "max_float" Double
                :> QueryParam "rarity" Int
                :> QueryParam "paint_seed" Int
                :> QueryParam "paint_index" Int
                :> QueryParam "user_id" Int
                :> QueryParam "collection" Text
                :> QueryParam "min_price" Int
                :> QueryParam "max_price" Int
                :> QueryParam "market_hash_name" Text
                :> QueryParam "type" CSFloatType
                :> QueryParam "stickers" Text
                :> Get '[JSON] GetListingsResponse
    }
    deriving (Generic)
