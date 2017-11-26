module Peanocoin.Ledger where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Monoid (mempty)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map

import Peanocoin.Address as Address
import Peanocoin.Transaction as Tx
import Peanocoin.Block (Block(..))
import Peanocoin.Blockchain as Blockchain


type Ledger
    = Map Address.Address Int

data TransferError
    = AddressDoesNotExist Address.Address
    | InvalidPublicKey
    | InvalidTransferAmount Int
    | InsufficientBalance Address.Address Int

derive instance genericTransferError :: Generic TransferError _
derive instance eqTransferError :: Eq TransferError

instance showTransferError :: Show TransferError where
    show = genericShow


buildLedger :: Blockchain.Blockchain -> Either TransferError Ledger
buildLedger chain =
    foldM applyBlock mempty chain

applyBlock :: Ledger -> Block -> Either TransferError Ledger
applyBlock ledger (Block block) =
    foldM applyTransaction ledger block.transactions

applyTransaction :: Ledger -> Tx.Transaction -> Either TransferError Ledger
applyTransaction ledger (Tx.Transaction transaction) = do
    case transaction.header of
        Tx.TransferHeader header -> applyTransfer header
        Tx.RewardHeader   header -> applyReward header

    where
        applyTransfer { amount, senderKey, recipient } =
            case Address.pkToAddress senderKey of
                Just address ->
                    updateBalance address recipient amount ledger
                Nothing ->
                    Left InvalidPublicKey

        applyReward { reward, minerKey } =
            case Address.pkToAddress minerKey of
                Just address ->
                     pure $ transferAmount address reward ledger
                Nothing ->
                    Left InvalidPublicKey




currentBalance :: Address.Address -> Ledger -> Maybe Int
currentBalance = Map.lookup

updateBalance :: Address.Address -> Address.Address -> Int -> Ledger -> Either TransferError Ledger
updateBalance from to amount ledger = do
    when (amount < 1) $ Left (InvalidTransferAmount amount)

    let current = currentBalance from ledger

    case current of
        Just balance -> do
            let newBalance = balance - amount
            when (newBalance < 0) $ Left (InsufficientBalance from balance)

            pure $ ledger
                # Map.insert from newBalance
                # transferAmount to amount

        Nothing ->
            Left (AddressDoesNotExist from)


transferAmount :: Address.Address -> Int -> Ledger -> Ledger
transferAmount address amount ledger =
    let
        recipientBalance = maybe amount (_ + amount) (currentBalance address ledger)
    in
    Map.insert address recipientBalance ledger
