module Peanocoin.Describe where

import Prelude

import Data.Array as Array
import Peanocoin.Address (Address(..))
import Peanocoin.Block (InvalidBlock(..))
import Peanocoin.Block as Block
import Peanocoin.Blockchain (FindBlockError(..))
import Peanocoin.Ledger (TransferError(..))
import Peanocoin.Node (Error(..), Event(..))
import Peanocoin.Transaction (InvalidTransaction(..))
import Peanocoin.Transaction as Tx



event :: Event -> String
event = case _ of
    ReceiveBlock block ->
        "Block " <> (Block.hashBlock block) <> " is valid and added to the blockchain"

    ReceiveTx tx ->
        "Transaction " <> (Tx.hashTransaction tx) <> "is valid and added to the mempool"

    ReceivePeers peers ->
        (show $ Array.length peers) <> " peers added to the list of known peers"

    PeerDiscovered peer ->
        "New peer discovered, check if we are up to date"


error :: Error -> String
error = case _ of
    ErrorBlock err ->
        invalidBlockError err
    ErrorTx err ->
        invalidTxError err
    ErrorBlockNotFound err ->
        findBlockError err


transferError :: TransferError -> String
transferError = case _ of
    AddressDoesNotExist (Address address) ->
        "Address does not exist: " <> address
    InvalidPublicKey ->
        "Invalid Public Key"
    InvalidTransferAmount amount ->
        "Transfer amount " <> (show amount) <> " is invalid"
    InsufficientBalance (Address address) balance ->
        "Current balance of address " <> address <> "is: " <> (show balance)
        <> " which is insufficient for the transfer requested"


findBlockError :: FindBlockError -> String
findBlockError = case _ of
    FindBlockInvalidHash hash ->
        "Block not found with this hash: " <> hash
    FindBlockNoMoreBlocks ->
        "There are no more blocks!"


invalidBlockError :: InvalidBlock -> String
invalidBlockError = case _ of
    InvalidBlockIndex ->
        "Block index is invalid"
    InvalidBlockHash ->
        "Block hash is invalid"
    InvalidBlockMerkleRoot ->
        "Block merkle root is invalid"
    InvalidBlockNoReward ->
        "Block has no reward transaction"
    InvalidRewardTxCount ->
        "Block has too many reward transactions"
    InvalidRewardTxPosition ->
        "Block has reward transaction in a wrong position. It should be the last"
    InvalidRewardTx given expected ->
        "Reward transaction has an unexpected amount. Was expecting: "
        <> (show expected)
        <> " but received: " <> (show given)
    InvalidPrevBlockHash ->
        "Previous block hash is invalid"
    InvalidBlockTx err ->
        invalidTxError err
    InvalidEmptyBlockchain ->
        "Blockchain is empty"


invalidTxError :: InvalidTransaction -> String
invalidTxError = case _ of
    InvalidTransactionSignature ->
        "Transaction signature is invalid"
    TxMustBeTransfer ->
        "Only transactions of type Transfer are accepted"
    TxAlreadyInMemPool ->
        "Transaction is already known and in the mempool"
    TxAlreadyInBlock hash ->
        "Transaction is already known and mined in block: " <> hash
