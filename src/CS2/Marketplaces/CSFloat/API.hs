module CS2.Marketplaces.CSFloat.API where

import CS2.Marketplaces.CSFloat.Types
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import CS2.Marketplaces.CSFloat.RateLimit (RateLimitHeaders)

data CSFloatAPI mode = CSFloatAPI
    { listingsAPI ::
        mode
            :- "api"
                :> "v1"
                :> "listings"
                :> NamedRoutes ListingsAPI
    , getMe ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "me"
                :> Get '[JSON] Me
    , getInventory ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "me"
                :> "inventory"
                :> Get '[JSON] [Item]
    , getStall ::
        mode
            :- "api"
                :> "v1"
                :> "users"
                :> Capture "UserID" Text
                :> "stall"
                :> QueryParam "cursor" Text
                :> QueryParam "limit" Int
                :> Get '[JSON] Stall
    , getSales ::
        mode
            :- "api"
                :> "v1"
                :> "history"
                :> Capture "market_hash_name" Text
                :> "sales"
                :> QueryParam "paint_index" Int
                :> Get '[JSON] [Listing]
    , getTransactions ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "me"
                :> "transactions"
                :> QueryParam "page" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] Transactions
    , getWatchlist ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "me"
                :> "watchlist"
                :> QueryParam "limit" Int
                :> Get '[JSON] Watchlist
    , deleteWatchlist ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "listings"
                :> Capture "listingId" Int
                :> "watchlist"
                :> Delete '[JSON] ()
    , getTradeHistory ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> "api"
                :> "v1"
                :> "me"
                :> "trades"
                :> QueryParam "role" Text
                :> QueryParam "page" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] Trades
    }
    deriving (Generic)

data ListingsAPI mode = ListingsAPI
    { getListings ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> QueryParam "cursor" Text
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
                :> Get '[JSON] GetListingsResponse
    , getListing ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> Capture "listingId" Int
                :> Get '[JSON] (RateLimitHeaders Listing)
    , postListing ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> ReqBody '[JSON] PostListingRequest
                :> Post '[JSON] Listing
    , deleteListing ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> Capture "listingId" Text
                :> Delete '[JSON] () -- reevaluate return type
    , updateListingPrice ::
        mode
            :- Header' '[Required] "Authorization" Text
                :> Capture "listingId" Text
                :> ReqBody '[JSON] UpdateListingPriceRequest
                :> Patch '[JSON] Listing
    }
    deriving (Generic)
