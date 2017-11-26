module Peanocoin.Blockchain where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Peanocoin.Transaction as Tx
import Peanocoin.Block (Block(..))
import Peanocoin.Block as Block
import Peanocoin.Hash (Hash)


type Blockchain
    = Array Block


data FindBlockError
    = FindBlockInvalidHash Hash
    | FindBlockNoMoreBlocks

derive instance eqFindBlockError :: Eq FindBlockError
derive instance genericFindBlockError :: Generic FindBlockError _

instance showFindBlockError :: Show FindBlockError where
    show = genericShow


blockMatchHash :: Hash -> Block -> Boolean
blockMatchHash hash block =
    Block.hashBlock block == hash


findBlock :: Blockchain -> Hash -> Either FindBlockError Block
findBlock chain hash =
    Either.note (FindBlockInvalidHash hash) $ Array.find (blockMatchHash hash) chain


findBlockAfter :: Blockchain -> Hash -> Either FindBlockError Block
findBlockAfter chain hash = do
    index <- Either.note
        (FindBlockInvalidHash hash)
        $ Array.findIndex (blockMatchHash hash) chain
    
    Either.note FindBlockNoMoreBlocks $ Array.index chain (index + 1)


txInBlockchain :: Blockchain -> Hash -> Maybe { block :: Block, tx :: Tx.Transaction }
txInBlockchain blockchain hash =
    let
        find acc block =
            case acc of
                Just _  -> acc
                Nothing ->
                    let
                        (Block { transactions }) = block
                    in
                    Array.find (Tx.txMatchHash hash) transactions
                        # map (\txFound -> { block, tx: txFound })
    in
    Array.foldl find Nothing blockchain


