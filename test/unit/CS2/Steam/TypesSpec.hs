module CS2.Steam.TypesSpec (spec) where

import CS2.Steam.Types
import Data.Aeson (decode)
import Test.Hspec

-- | Codec tests for "CS2.Steam.Types".
spec :: Spec
spec = do
    enumSpec

enumSpec :: Spec
enumSpec = describe "Enum spec:" $ do
    describe "SteamTradeOfferState:" $ do
        it "decodes every documented value" $ do
            decode "1" `shouldBe` Just Invalid
            decode "2" `shouldBe` Just Active
            decode "3" `shouldBe` Just Accepted
            decode "4" `shouldBe` Just Countered
            decode "5" `shouldBe` Just Expired
            decode "6" `shouldBe` Just Canceled
            decode "7" `shouldBe` Just Declined
            decode "8" `shouldBe` Just InvalidItems
            decode "9" `shouldBe` Just CreatedNeedsConfirmation
            decode "10" `shouldBe` Just CanceledBySecondFactor
            decode "11" `shouldBe` Just InEscrow
        it "rejects an out-of-range code" $ do
            (decode "0" :: Maybe SteamTradeOfferState) `shouldBe` Nothing
