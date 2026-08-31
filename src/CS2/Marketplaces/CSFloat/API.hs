module CS2.Marketplaces.CSFloat.API where

import CS2.Marketplaces.CSFloat.RateLimit (WithHeaders)
import CS2.Marketplaces.CSFloat.Types
import Data.Text
import GHC.Generics (Generic)
import Servant.API

data CSFloatAPI mode = CSFloatAPI
    { listingsApi ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "listings"
                :> NamedRoutes ListingsAPI
    , meApi ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "me"
                :> NamedRoutes MeAPI
    , usersApi ::
        mode
            :- "api"
                :> "v1"
                :> "users"
                :> NamedRoutes UsersAPI
    , historyApi ::
        mode
            :- "api"
                :> "v1"
                :> "history"
                :> NamedRoutes HistoryAPI
    }
    deriving (Generic)

data ListingsAPI mode = ListingsAPI
    { getListings ::
        mode
            :- QueryParam "cursor" Text
                :> QueryParam "limit" Int
                :> QueryParam "sort_by" SortBy
                :> QueryParam "category" Category
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
                :> QueryParam "type" Type
                :> QueryParam "stickers" Text
                :> Get '[JSON] (WithHeaders GetListingsResponse)
    , getListing ::
        mode
            :- Capture "listingId" Text
                :> Get '[JSON] (WithHeaders Listing)
    , postListing ::
        mode
            :- ReqBody '[JSON] PostListingRequest
                :> Post '[JSON] (WithHeaders Listing)
    , deleteListing ::
        mode
            :- Capture "listingId" Text
                :> Delete '[JSON] (WithHeaders NoContent)
    , updateListingPrice ::
        mode
            :- Capture "listingId" Text
                :> ReqBody '[JSON] UpdateListingPriceRequest
                :> Patch '[JSON] (WithHeaders Listing)
    , deleteWatchlist ::
        mode
            :- Capture "listingId" Text
                :> "watchlist"
                :> Delete '[JSON] (WithHeaders NoContent)
    }
    deriving (Generic)

data MeAPI mode = MeAPI
    { getMe :: mode :- Get '[JSON] (WithHeaders Me)
    , getInventory ::
        mode
            :- "inventory"
                :> Get '[JSON] (WithHeaders [Item])
    , getTransactions ::
        mode
            :- "transactions"
                :> QueryParam "page" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] (WithHeaders Transactions)
    , getWatchlist ::
        mode
            :- "watchlist"
                :> QueryParam "limit" Int
                :> Get '[JSON] (WithHeaders Watchlist)
    , getTradeHistory ::
        mode
            :- "trades"
                :> QueryParam "role" Text
                :> QueryParam "page" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] (WithHeaders Trades)
    }
    deriving (Generic)

data UsersAPI mode = UsersAPI
    { getStall ::
        mode
            :- Capture "UserID" Text
                :> "stall"
                :> QueryParam "cursor" Text
                :> QueryParam "limit" Int
                :> Get '[JSON] (WithHeaders Stall)
    }
    deriving (Generic)

data HistoryAPI mode = HistoryAPI
    { getSales ::
        mode
            :- Capture "market_hash_name" Text
                :> "sales"
                :> QueryParam "paint_index" Int
                :> Get '[JSON] (WithHeaders [Listing])
    }
    deriving (Generic)
