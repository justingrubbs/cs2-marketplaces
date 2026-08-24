module CS2.Marketplaces.CSFloat.RateLimit (RateLimitHeaders)
where

import Data.Text (Text)
import Servant.API

type RateLimitHeaders =
    Headers
        '[ Header "x-ratelimit-limit" Text
         , Header "x-ratelimit-remaining" Text
         , Header "x-ratelimit-reset" Text
         ]



