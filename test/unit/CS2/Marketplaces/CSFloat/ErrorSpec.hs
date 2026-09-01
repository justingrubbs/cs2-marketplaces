module CS2.Marketplaces.CSFloat.ErrorSpec (spec) where

import Test.Hspec

-- | 'CS2.Marketplaces.CSFloat.Error.fromClientError' status-code mapping and
-- 'ApiError' body decoding. Pure: build 'ClientError' values by hand.
spec :: Spec
spec = do
    it "maps each documented status code to its CSFloatError constructor" pending
    it "maps an unlisted status to UnexpectedStatus with the raw code" pending
    it "decodes a JSON error body into ApiError, tolerating non-JSON bodies" pending
    it "maps ConnectionError to NetworkError and decode failures to InvalidResponse" pending
