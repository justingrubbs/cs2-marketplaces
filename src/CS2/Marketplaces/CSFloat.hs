module CS2.Marketplaces.CSFloat where

import CS2.Marketplaces.CSFloat.Types
import Data.Text
import GHC.Generics (Generic)
import Servant.API

data CSFloatConfig = CSFloatConfig {apiKey :: Text}

data CSFloatAPI mode = CSFloatAPI
    { getListings :: mode :- "api" :> "v1" :> "listings" :> ()
    }
    deriving (Generic)
