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

module Week08.TokenSale
    ( TokenSale (..)
    , TSRedeemer (..)
    , nftName
    , TSStartSchema
    , TSStartSchema'
    , TSUseSchema
    , startEndpoint
    , startEndpoint'
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract hiding (when)
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.Currency    as C
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..), uncurry)
import qualified Prelude

-- Data type to encapsulate the neccesary parts of the TokenSale
data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash
    , tsToken  :: !AssetClass
    , tsNFT    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''TokenSale

-- The redeemers, which will be used as the endpoints
data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close 
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

-- A helper function to go from some value to an integer(lovelaces)
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue



-- ** This is the state transition function ** -- 
-- Its goal: Establish the different ways in which the StateMachine(SM) can transition states
-- How?: Apply constraints to tranistions, and specify what the state will be AFTER the transition
-- Important things to note: The NFT which is used to identify the correct UTxO is passed along with the State(Data,Value) in the form of the Value
-- It automatically detects the correct UTxO so there is no need to search for it or check that it is there.
--
-- Part of the Homework:
-- Use a Maybe interger for the price datum to determine if the TokenSale is open( has not been closed)
-- Change all uses of stateData s to Just _
{-# INLINABLE transition #-}
transition :: TokenSale -> State (Maybe Integer) -> TSRedeemer -> Maybe (TxConstraints Void Void, State (Maybe Integer))
transition ts s r = case (stateValue s, stateData s, r) of
    (v, Just _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State (Just p) $
                                                      v 				      <>
                                                      nft (negate 1)
                                                    )
    (v, Just p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State (Just p) $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    (v, Just p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State (Just p) $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, Just p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State (Just p) $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
-- The endpoint close can only be called by the original TokenSalen creator.
-- Once the endpoint is called the next state for the StateMachine is a datum of Nothing and a mempty value
    (v, Just _, Close) 				-> Just ( Constraints.mustBeSignedBy (tsSeller ts)
							, State Nothing $
							  mempty
						   )
    _                                       -> Nothing
  where
    nft :: Integer -> Value
    nft = assetClassValue (tsNFT ts)


-- Creates a SM with the TokenSale's tsNFT as a unique identifier
{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine (Maybe Integer) TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) isNothing

-- Takes in the TokenSale, Datum, Redeemer, and script context(Sounds like a regular script validator)
-- and uses the mkValidator composed with the above function to create a validator script which returns a bool
{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

-- A type of TS which is SM which has a datum and redeemer
type TS = StateMachine (Maybe Integer) TSRedeemer

-- Compile the inlinable code to a plutus script so that it can be used on chain
tsInst :: TokenSale -> Scripts.ScriptInstance TS
tsInst ts = Scripts.validator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

-- ** HERE STARTS THE OFF CHAIN CODE ** --

-- Using the script tsInst create a off chain version of the validator script
tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsInst

-- Use the above script to generate an address
tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

-- Make a client so that interaction with the SM is possible
tsClient :: TokenSale -> StateMachineClient (Maybe Integer) TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsInst ts)

-- Convert a currency error in the contract to a text error
mapErrorC :: Contract w s C.CurrencyError a -> Contract w s Text a
mapErrorC = mapError $ pack . show

-- Convert a SM error in the contract to a text error
mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

-- Helper function to name the unique NFT
nftName :: TokenName
nftName = "NFT"

-- This function will establish an initial TokenSale SM
-- Case 1: An nft exists at the initiators PubKeyHash, use this.
-- Case 2: No nft exists at the initiators PubKeyHAsh, create one
startTS :: HasBlockchainActions s => Maybe CurrencySymbol -> AssetClass -> Contract (Last TokenSale) s Text TokenSale
startTS mcs token = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    cs  <- case mcs of
        Nothing  -> C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(nftName, 1)])
        Just cs' -> return cs'
    let ts = TokenSale {-Populate the TokenSale datatype-}
            { tsSeller = pkh
            , tsToken  = token
            , tsNFT    = AssetClass (cs, nftName)
            }
        client = tsClient ts {-Create the client-}
    void $ mapErrorSM $ runInitialise client (Just 0) mempty {-Use the client to initialize the SM-}
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts
    return ts

-- ** The 5 endpoints which can be used to access the SM transitions ** --
setPrice :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

buyTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: HasBlockchainActions s => TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

-- Add in the SMclient communication with the close endpoint (Part of Homeowork)
close :: HasBlockchainActions s => TokenSale -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close

-- Two start is used twice because the wallet may not have the NFT to start the TokenSale
-- These Schemas are used to add onto the 
type TSStartSchema = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, TokenName)
type TSStartSchema' = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, CurrencySymbol, TokenName)
type TSUseSchema = BlockchainActions
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      () {-Add in the close endpoint (part of homework)-}

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = startTS' >> startEndpoint
  where
    startTS' = handleError logError $ endpoint @"start"  >>= void . startTS Nothing . AssetClass

startEndpoint' :: Contract (Last TokenSale) TSStartSchema' Text ()
startEndpoint' = startTS' >> startEndpoint'
  where
    startTS' = handleError logError $ endpoint @"start"  >>= \(cs1, cs2, tn) ->  void $ startTS (Just cs1) $ AssetClass (cs2, tn)

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints ts = (setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close') >> useEndpoints ts
  where
    setPrice'  = handleError logError $ endpoint @"set price"  >>= setPrice ts
    addTokens' = handleError logError $ endpoint @"add tokens" >>= addTokens ts
    buyTokens' = handleError logError $ endpoint @"buy tokens" >>= buyTokens ts
    withdraw'  = handleError logError $ endpoint @"withdraw"   >>= uncurry (withdraw ts)
    close'     = handleError logError $ endpoint @"close"      >> close ts
