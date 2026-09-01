module CS2.Steam.Types where

import Data.Aeson (FromJSON (..), withScientific)

data SteamTradeOfferState
    = Invalid
    | Active
    | Accepted
    | Countered
    | Expired
    | Canceled
    | Declined
    | InvalidItems
    | CreatedNeedsConfirmation
    | CanceledBySecondFactor
    | InEscrow
    deriving (Show, Eq, Ord)

instance FromJSON SteamTradeOfferState where
    parseJSON = withScientific "SteamTradeOfferState" $ \n -> case round n :: Int of
        1 -> pure Invalid
        2 -> pure Active
        3 -> pure Accepted
        4 -> pure Countered
        5 -> pure Expired
        6 -> pure Canceled
        7 -> pure Declined
        8 -> pure InvalidItems
        9 -> pure CreatedNeedsConfirmation
        10 -> pure CanceledBySecondFactor
        11 -> pure InEscrow
        other -> fail $ "Unknown SteamTradeOfferState: " <> show other
