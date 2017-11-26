module Peanocoin.MemPool where


import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

import Peanocoin.Block as Block
import Peanocoin.Transaction as Tx
import Peanocoin.Hash (Hash)


type MemPool
    = Array Tx.Transaction


find :: MemPool -> Hash -> Maybe Tx.Transaction
find memPool hash =
    Array.find (Tx.txMatchHash hash) memPool


-- Purge mined transactions from MemPool
purgeMemPool :: MemPool -> Block.Block -> MemPool
purgeMemPool memPool (Block.Block { transactions }) =
    Array.filter purge memPool
    where
        purge tx =
            let
                hash  = Tx.hashTransaction tx
                found = Array.find (Tx.txMatchHash hash) transactions
            in
            case found of
                Just _  -> false
                Nothing -> true

