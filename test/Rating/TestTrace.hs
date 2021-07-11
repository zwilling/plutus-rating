{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Rating.TestTrace where

import           Control.Monad          hiding (fmap)
-- import           Control.Monad.Freer.Extras as Extras
-- import           Data.Default               (Default (..))
import qualified Data.Map               as Map
import           Data.Monoid            (Last (..))
import           Data.Text              (Text)
import           Ledger
import           Ledger.Ada             as Ada
import           Ledger.Value           as Value
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import           Prelude                (IO, Semigroup (..))
import           Wallet.Emulator.Wallet

import           Rating.Rating

test :: IO ()
-- test = runEmulatorTraceIO' def emCfg myTrace
test = runEmulatorTraceIO myTrace
{-  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 2]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000
-}
myTrace :: EmulatorTrace ()
myTrace = do
    let op = RatingActionParams
                {
                -- TODO include other contracts to have script addresses to rate
                rapScriptAddress = pubKeyHash $ walletPubKey $ Wallet 1
                , rapScore = 5
                }

    h1 <- activateContractWallet (Wallet 1) $ createRating op

    void $ Emulator.waitNSlots 1

    void $ Emulator.waitNSlots 2
