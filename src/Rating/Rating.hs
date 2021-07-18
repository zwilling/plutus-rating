{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Rating.Rating where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Map             as Map
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Ada           as Ada
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (Semigroup (..), Show, String, show)
import           Text.Printf          (printf)

data RatingParam = RatingParam
    {
        -- | The Address of the script being rated on
        ratedScriptAddress :: !PubKeyHash
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''RatingParam
PlutusTx.makeLift ''RatingParam

data RatingRedeemer =
    -- | Edit an existing rating to a new value
    Edit Integer
    -- | Removing a rating
    | Delete
    deriving (Show)

PlutusTx.unstableMakeIsData ''RatingRedeemer

{--|
    Datum of rating UTXOs containing the rating score (1-5) and the PubKeyHash of the person rating
-}
data RatingDatum = RatingDatum Integer !PubKeyHash
    deriving Show

PlutusTx.unstableMakeIsData ''RatingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: RatingParam -> RatingDatum -> RatingRedeemer -> ScriptContext -> Bool
mkValidator ratingRaram ratingDatum ratingRedeemer ctx = False

data Rating
instance Scripts.ScriptType Rating where
    type instance DatumType Rating = RatingDatum
    type instance RedeemerType Rating = RatingRedeemer

inst :: RatingParam -> Scripts.ScriptInstance Rating
inst p = Scripts.validator @Rating
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RatingDatum @RatingRedeemer

validator :: RatingParam -> Validator
validator = Scripts.validatorScript . inst

scrAddress :: RatingParam -> Ledger.Address
scrAddress = scriptAddress . validator

{-|
    Parameters to prepare a rating transaction
-}
data RatingActionParams = RatingActionParams
    {
    -- | The script to be rated
    rapScriptAddress :: !PubKeyHash
    -- | The rating score to give
    , rapScore       :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

data EditActionParams = EditActionParams
    {
    -- | The script to be rated
    rapScriptAddress2 :: !PubKeyHash
    -- | The new rating score
    , newRapScore     :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

type RatingSchema =
    BlockchainActions
        .\/ Endpoint "createRating" RatingActionParams
        .\/ Endpoint "editRating" RatingActionParams
        -- .\/ Endpoint "deleteRating" !PubKeyHash

createRating :: RatingActionParams -> Contract (Last RatingParam) RatingSchema Text ()
createRating rapParams = do
    ownKey <- pubKeyHash <$> ownPubKey
    let p  = RatingParam
                { ratedScriptAddress = rapScriptAddress rapParams
                }
        ratingDatum = RatingDatum (rapScore rapParams) ownKey
        -- ToDo: create rating utxo with datum to submitt transaction
        tx = mustPayToTheScript ratingDatum $ Ada.lovelaceValueOf 1
    ledgerTx <- submitTxConstraints (inst p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Send test transaction to rate %s"
        (show $ rapScriptAddress rapParams)

editRating :: EditActionParams -> Contract (Last RatingParam) RatingSchema Text ()
editRating rapParams = do
    ownKey <- pubKeyHash <$> ownPubKey
    ratingUtxos <- utxoAt $ scrAddress RatingParam{ ratedScriptAddress = rapScriptAddress2 rapParams}
    let p  = RatingParam
            { ratedScriptAddress = rapScriptAddress2 rapParams
            }

        newRatingDatum = RatingDatum (newRapScore rapParams) ownKey

        tx = case find f $ Map.toList ratingUtxos of
            Nothing -> throwError "user hasn't rated %s yet" (show $ rapScriptAddress2 rapParams)
            Just (oref, o) -> mustSpendScriptOutput oref Edit $ newRapScore rapParams <>
                                mustPayToTheScript newRatingDatum $ Ada.lovelaceValueOf 1

    ledgerTx <- submitTxConstraints (inst p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Send test transaction to edit rating of %s"
        (show $ rapScriptAddress2 rapParams)

    where

        f :: (TxOutRef, TxOutTx) -> Bool
        f (_, o) =  isSuitable o

        isSuitable :: TxOutTx -> Bool
        isSuitable o = case txOutDatumHash $ txOutTxOut o of
            Nothing -> False
            Just h -> case Map.lookup h $ txData $txOutTxTx o of
                Nothing -> False
                Just (Datum e) -> case PlutusTx.fromData e of
                    Nothing                -> False
                    Just RatingDatum _ key -> key == ownKey
                    Just _                 -> False

-- ToDo: implement deleteRating

endpoints :: Contract (Last RatingParam) RatingSchema Text ()
endpoints = createRating' >> endpoints
  where
    createRating' = endpoint @"createRating" >>= createRating
    editRating' = endpoint @"editRating" >>= grab
    -- deleteRating' = endpoint @"deleteRating" >>= grab
