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
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

data RatingParam = RatingParam
    { 
    } deriving Show

PlutusTx.unstableMakeIsData ''RatingParam
PlutusTx.makeLift ''RatingParam

{-# INLINABLE mkValidator #-}
mkValidator :: RatingParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx = False

data Rating
instance Scripts.ScriptType Rating where
    type instance DatumType Rating = ()
    type instance RedeemerType Rating = ()

inst :: RatingParam -> Scripts.ScriptInstance Rating
inst p = Scripts.validator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: RatingParam -> Validator
validator = Scripts.validatorScript . inst

scrAddress :: RatingParam -> Ledger.Address
scrAddress = scriptAddress . validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" Slot

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let p  = VestingParam
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx = mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (inst p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Slot -> Contract w s e ()
grab d = do
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    if now < d
        then logInfo @String $ "too early"
        else do
            let p = VestingParam
                        { beneficiary = pkh
                        , deadline    = d
                        }
            utxos <- utxoAt $ scrAddress p
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos      <>
                                  Constraints.otherScript (validator p)
                        tx :: TxConstraints Void Void
                        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                                  mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
