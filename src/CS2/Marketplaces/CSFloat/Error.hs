module CS2.Marketplaces.CSFloat.Error where

import Control.Exception (displayException)
import Data.Aeson (FromJSON, eitherDecode) -- eitherDecode is the addition
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client.Core (ClientError (..), ResponseF (..))

data ApiError = ApiError
    { code :: Maybe Int
    , message :: Text
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON)

-- | Sourced from official CSFloat API documentation.
data CSFloatError
    = -- | 400 — the request was malformed or otherwise rejected.
      BadRequest (Maybe ApiError)
    | -- | 401 — API key missing or wrong.
      Unauthorized (Maybe ApiError)
    | -- | 403 — authenticated, but not permitted.
      Forbidden (Maybe ApiError)
    | -- | 404 — the resource does not exist.
      NotFound (Maybe ApiError)
    | -- | 405 — method not allowed on this route.
      MethodNotAllowed (Maybe ApiError)
    | -- | 406 — a non-JSON response format was requested.
      NotAcceptable (Maybe ApiError)
    | -- | 410 — the resource was permanently removed.
      Gone (Maybe ApiError)
    | -- | 429 — CSFloat's own rate limit was hit.
      TooManyRequests (Maybe ApiError)
    | -- | 500 — CSFloat server error.
      InternalServerError (Maybe ApiError)
    | -- | 503 — CSFloat down for maintenance.
      ServiceUnavailable (Maybe ApiError)
    | -- | Any other non-2xx status, with the raw code.
      UnexpectedStatus Int (Maybe ApiError)
    | -- | A 2xx response whose body did not match the expected schema.
      InvalidResponse Text
    | -- | CSFloat could not be reached at all (DNS, TLS, timeout, reset).
      NetworkError Text
    | -- | The client-side rate limiter can never satisfy this call's toll.
      ClientLimited Text
    deriving stock (Show, Eq)

-- | Translate a @servant-client@ 'ClientError' into a 'CSFloatError'.
fromClientError :: ClientError -> CSFloatError
fromClientError = \case
    FailureResponse _ response ->
        let body = decodeApiError (responseBody response)
         in case statusCode (responseStatusCode response) of
                400 -> BadRequest body
                401 -> Unauthorized body
                403 -> Forbidden body
                404 -> NotFound body
                405 -> MethodNotAllowed body
                406 -> NotAcceptable body
                410 -> Gone body
                429 -> TooManyRequests body
                500 -> InternalServerError body
                503 -> ServiceUnavailable body
                other -> UnexpectedStatus other body
    DecodeFailure detail _ -> InvalidResponse detail
    UnsupportedContentType mediaType _ ->
        InvalidResponse ("unexpected content-type: " <> T.pack (show mediaType))
    InvalidContentTypeHeader _ -> InvalidResponse "missing or invalid content-type header"
    ConnectionError err -> NetworkError (T.pack (displayException err))
  where
    decodeApiError :: LBS.ByteString -> Maybe ApiError
    decodeApiError = either (const Nothing) Just . eitherDecode
