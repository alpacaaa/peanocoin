module Peanocoin.Node where

import Prelude

import Peanocoin.Block (Block, InvalidBlock(..))
import Peanocoin.Transaction (Transaction, InvalidTransaction(..))
import Peanocoin.Blockchain (FindBlockError(..))
import Peanocoin.Blockchain (Blockchain)
import Peanocoin.MemPool (MemPool)

import Peanocoin.Block as Block
import Peanocoin.Blockchain as Blockchain
import Peanocoin.MemPool as MemPool
import Peanocoin.Transaction as Tx

import Crypto.Simple as Crypto
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Peanocoin.Hash (Hash)


type Peer = String


data Event
    = ReceivePeers   (Array Peer)
    | ReceiveBlock   Block
    | ReceiveTx      Transaction


data Effect
    = SendBlock         Block
    | SendTx            Transaction
    | RequestPeers      Peer -- my ip
    | RequestBlockAfter Hash


data State = State
    { keyPair    :: Crypto.KeyPair
    , host       :: Peer            -- this node own public address
    , name       :: String
    , peers      :: Set Peer
    , blockchain :: Blockchain
    , memPool    :: MemPool
    }

data Error
    = ErrorBlock InvalidBlock
    | ErrorTx    InvalidTransaction
    | ErrorBlockNotFound FindBlockError



type Result =
    Either Error { state :: State, effect :: Maybe Effect }


noEffects :: State -> Result
noEffects state = Right { state, effect: Nothing }


withEffect :: State -> Effect -> Result
withEffect state effect = Right { state, effect: Just effect }


handlePeers :: State -> Array Peer -> Result
handlePeers (State state) ips =
    let
        newIps   = Set.fromFoldable ips
                 # Set.delete state.host
        peers    = Set.union state.peers newIps
        newState = state { peers = peers }

        requestNextBlock block
            = withEffect
                (State newState)
                (RequestBlockAfter $ Block.hashBlock block)

        reject err
            = Left (ErrorBlock err)

        tip = Either.note
            InvalidEmptyBlockchain
            (Array.last state.blockchain)
    in
    Either.either reject requestNextBlock tip


handleBlock :: State -> Block -> Result
handleBlock (State state) block =
    Either.either rejectBlock appendBlock validate
    where
        validate = do
            lastBlock <- Either.note InvalidEmptyBlockchain (Array.last state.blockchain)
            Block.validateBlock { prev: lastBlock, current: block }

        appendBlock _ =
            let
                newState = state 
                    { blockchain = Array.snoc state.blockchain block
                    , memPool    = MemPool.purgeMemPool state.memPool block
                    }
            in
            withEffect (State newState) (SendBlock block)

        rejectBlock err =
            Left (ErrorBlock err)


handleTransaction :: State -> Transaction -> Result
handleTransaction (State state) tx
    | Tx.isRewardTx tx
        = Left (ErrorTx TxMustBeTransfer)

    | Maybe.isJust $ MemPool.find state.memPool (Tx.hashTransaction tx)
        = Left (ErrorTx TxAlreadyInMemPool)

    | otherwise
        = Either.either reject accept validate
        where
            txHash   = Tx.hashTransaction tx

            validate =
                case Blockchain.txInBlockchain state.blockchain txHash of
                    Just { block } ->
                        Left $ TxAlreadyInBlock (Block.hashBlock block)
                    Nothing ->
                        Tx.validateTransactionSig tx

            accept _ =
                let
                    newState = state { memPool = Array.snoc state.memPool tx }
                in
                withEffect (State newState) (SendTx tx)

            reject err
                = Left (ErrorTx err)


updateState :: State -> Event -> Result
updateState state = case _ of
    ReceivePeers nodes ->
        handlePeers state nodes

    ReceiveBlock block ->
        handleBlock state block

    ReceiveTx tx ->
        handleTransaction state tx



