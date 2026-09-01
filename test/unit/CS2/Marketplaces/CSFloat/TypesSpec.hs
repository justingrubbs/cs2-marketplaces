module CS2.Marketplaces.CSFloat.TypesSpec (spec) where

import CS2.Marketplaces.CSFloat.Types
import Data.Aeson (decode)
import Servant.API (toQueryParam)
import Test.Hspec

-- | Codec tests for "CS2.Marketplaces.CSFloat.Types".
spec :: Spec
spec = do
    enumSpec
    recordSpec

{- | Scalar codecs for the sum types: 'FromJSON' truth tables (known value ->
named constructor; unknown -> @*Other@ carrying the raw text, or decode failure
for the closed enums) and 'ToHttpApiData' truth tables (every constructor ->
its exact wire string). Inputs are inline literals.
-}
enumSpec :: Spec
enumSpec = describe "Enum spec:" $ do
    describe "SortBy:" $ do
        it "encodes every constructor to its documented `sort_by` string" $ do
            toQueryParam LowestPrice `shouldBe` "lowest_price"
            toQueryParam HighestPrice `shouldBe` "highest_price"
            toQueryParam MostRecent `shouldBe` "most_recent"
            toQueryParam ExpiresSoon `shouldBe` "expires_soon"
            toQueryParam LowestFloat `shouldBe` "lowest_float"
            toQueryParam HighestFloat `shouldBe` "highest_float"
            toQueryParam BestDeal `shouldBe` "best_deal"
            toQueryParam HighestDiscount `shouldBe` "highest_discount"
            toQueryParam FloatRank `shouldBe` "float_rank"
            toQueryParam NumBids `shouldBe` "num_bids"
    describe "Category:" $ do
        it "encodes every constructor to its documented `category` int-string" $ do
            toQueryParam Any `shouldBe` "0"
            toQueryParam Normal `shouldBe` "1"
            toQueryParam StatTrak `shouldBe` "2"
            toQueryParam Souvenir `shouldBe` "3"
    describe "Type:" $ do
        it "encodes every constructor to its documented `type` string" $ do
            toQueryParam BuyNow `shouldBe` "buy_now"
            toQueryParam Auction `shouldBe` "auction"
        it "decodes every documented value" $ do
            decode "\"buy_now\"" `shouldBe` Just BuyNow
            decode "\"auction\"" `shouldBe` Just Auction
        it "rejects an unknown string" $ do
            (decode "\"Squir rel\"" :: Maybe Type) `shouldBe` Nothing
    describe "ListingState:" $ do
        it "decodes every documented value" $ do
            decode "\"listed\"" `shouldBe` Just Listed
        it "puts any other value in `ListingStateOther` verbatim" $ do
            decode "\"Squir rel\"" `shouldBe` Just (ListingStateOther "Squir rel")
    describe "TransactionType:" $ do
        it "decodes every documented value" $ do
            decode "\"contract_purchased\"" `shouldBe` Just ContractPurchased
            decode "\"contract_sold\"" `shouldBe` Just ContractSold
            decode "\"deposit\"" `shouldBe` Just Deposit
            decode "\"trade_verified\"" `shouldBe` Just TradeVerified
        it "puts any other value in `TransactionTypeOther` verbatim" $ do
            decode "\"Squir rel\"" `shouldBe` Just (TransactionTypeOther "Squir rel")
    describe "TransactionDetailsType:" $ do
        it "decodes every documented value" $ do
            decode "\"buyer_ping\"" `shouldBe` Just BuyerPing
        it "puts any other value in `TransactionDetailsTypeOther` verbatim" $ do
            decode "\"Squir rel\"" `shouldBe` Just (TransactionDetailsTypeOther "Squir rel")
    describe "TradeState:" $ do
        it "decodes every documented value" $ do
            decode "\"verified\"" `shouldBe` Just Verified
        it "puts any other value in `TradeStateOther` verbatim" $ do
            decode "\"Squir rel\"" `shouldBe` Just (TradeStateOther "Squir rel")
    describe "TradeVerificationMode:" $ do
        it "decodes every documented value" $ do
            decode "\"escrow\"" `shouldBe` Just Escrow
            decode "\"inventory\"" `shouldBe` Just Inventory
        it "puts any other value in `TradeVerificationModeOther` verbatim" $ do
            decode "\"Squir rel\"" `shouldBe` Just (TradeVerificationModeOther "Squir rel")
    describe "ContractState:" $ do
        it "decodes every documented value" $ do
            decode "\"sold\"" `shouldBe` Just Sold
        it "puts any other value in `ContractStateOther` verbatim" $ do
            decode "\"Squir rel\"" `shouldBe` Just (ContractStateOther "Squir rel")

{- | Codecs for the product types. Response types are decode-only: golden-decode
a captured payload and spot-check fields, exercising the @data_@ / @type_@
relabels and missing-vs-null on optional fields. Request types are encode-only:
build a value and compare its JSON to an expected 'Value', pinning the tagged
'PostListingRequest' encoding and the treatment of @Nothing@ fields.
-}
recordSpec :: Spec
recordSpec = describe "record codecs" $ do
    describe "decode (responses)" $ pure ()
    describe "encode (requests)" $ pure ()
