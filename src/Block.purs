module Peanocoin.Block where


import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, getField, toObject)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode.Combinators ((:=), (~>))

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Bifunctor as Bifunctor
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap (StrMap)
import Data.String as String
import Data.Traversable as Traversable

import Crypto.Simple as Crypto
import Crypto.Hash.MerkleTree as MerkleTree
import Peanocoin.Transaction as Tx
import Peanocoin.Hash as Hash


type BlockHeader =
    { previousHash :: String    -- ^ Previous block hash
    , merkleRoot   :: String    -- ^ Merkle Root of transactions
    , nonce        :: Int       -- ^ Nonce for Proof-of-Work
    , difficulty   :: Int       -- ^ Number of 0 bits for Pow
    }

data Block = Block
    { index        :: Int           -- ^ Block height
    , header       :: BlockHeader   -- ^ Block header
    , transactions :: Array Tx.Transaction -- ^ List of Transactions
    }


data InvalidBlock
    = InvalidBlockIndex
    | InvalidBlockHash
    | InvalidBlockMerkleRoot
    | InvalidBlockNoReward
    | InvalidRewardTxCount
    | InvalidRewardTxPosition
    | InvalidRewardTx Int Int
    | InvalidPrevBlockHash
    | InvalidBlockTx Tx.InvalidTransaction
    | InvalidEmptyBlockchain

instance showBlock :: Show Block where
    show block@(Block { index }) = 
        [ "Block <index: "
        , show index
        , ", hash: "
        , hashBlock block
        ]
        # String.joinWith ""

instance eqBlock :: Eq Block where
    eq a b = (hashBlock a) == (hashBlock b)


instance encodeBlock :: EncodeJson Block where
    encodeJson block@(Block { index, header, transactions }) =
        (  "hash"         := hashBlock block
        ~> "index"        := index
        ~> "header"       := encodeBlockHeaderJson header
        ~> "transactions" := encodeJson transactions
        ~> jsonEmptyObject
        )


encodeBlockHeaderJson :: BlockHeader -> Json
encodeBlockHeaderJson header =
    (  "previousHash" := header.previousHash
    ~> "merkleRoot"   := header.merkleRoot
    ~> "nonce"        := header.nonce
    ~> "difficulty"   := header.difficulty
    ~> jsonEmptyObject
    )


instance decodeBlock :: DecodeJson Block where
    decodeJson json = do
        obj          <- Either.note "Unable to parse" $ toObject json

        index        <- getField obj "index"
        header       <- getField obj "header" >>= decodeBlockHeader
        transactions <- getField obj "transactions" >>= decodeJson
        
        pure $ Block
            { index
            , header
            , transactions
            }


decodeBlockHeader :: StrMap Json -> Either String BlockHeader
decodeBlockHeader obj = do
    nonce        <- getField obj "nonce"
    difficulty   <- getField obj "difficulty"
    previousHash <- getField obj "previousHash"
    merkleRoot   <- getField obj "merkleRoot"

    pure
        { previousHash
        , merkleRoot
        , nonce
        , difficulty
        }


hashBlock :: Block -> String
hashBlock (Block block) = hashBlockHeader block.header

hashBlockHeader :: BlockHeader -> String
hashBlockHeader header =
    let
        blob =
            header.previousHash
            <> header.merkleRoot
            <> show header.nonce
            <> show header.difficulty
    in
    Crypto.toString (Hash.hash blob)




validateBlockReward :: Array Tx.Transaction -> BlockHeader -> Either InvalidBlock Unit
validateBlockReward transactions header
    | Array.null transactions = Left InvalidBlockNoReward
    | otherwise               = do
        let rewardTxs = Array.filter Tx.isRewardTx transactions
        unless (Array.length rewardTxs == 1) $ Left InvalidRewardTxCount

        reward <- Either.note InvalidRewardTxCount $ Array.last transactions
        unless (Tx.isRewardTx reward) $ Left InvalidRewardTxPosition

        pure unit



createMerkleRoot :: Array Tx.Transaction -> String
createMerkleRoot transactions =
    map Tx.hashTransaction transactions
        # List.fromFoldable
        # MerkleTree.mkMerkleTree
        # MerkleTree.mtHash


mineBlock :: Array Tx.Transaction -> Crypto.KeyPair -> Block -> Maybe Block
mineBlock txPool { private, public } (Block prevBlock) = do
    -- TODO determine reward
    let rewardHeader = Tx.RewardHeader { minerKey: public, reward: 100 }

    signature <- Crypto.sign private (Tx.hashTransactionHeader rewardHeader)

    let
        rewardTx = Tx.Transaction
            { header: rewardHeader
            , signature: signature
            }

        transactions = Array.snoc txPool rewardTx

        -- TODO determine difficulty
        header = proofOfWork
            { previousHash: hashBlockHeader prevBlock.header
            , merkleRoot: createMerkleRoot transactions
            , nonce: 0
            , difficulty: prevBlock.header.difficulty
            }

        headerHash = hashBlockHeader header

    pure $ Block
        { index: prevBlock.index + 1
        , header
        , transactions
        }


proofOfWorkMatch :: Int -> String -> Boolean
proofOfWorkMatch difficulty hash =
    let
        proof = String.count (_ == '0') hash
    in
    proof >= difficulty

proofOfWork :: BlockHeader -> BlockHeader
proofOfWork header =
    let
        newHeader  = header { nonce = header.nonce + 1 }
        headerHash = hashBlockHeader newHeader
    in
    if proofOfWorkMatch header.difficulty headerHash
        then newHeader
        else proofOfWork newHeader


validateBlock :: { prev :: Block, current :: Block } -> Either InvalidBlock Unit
validateBlock { prev: Block prevBlock, current: Block block }
    | block.index /= prevBlock.index + 1                                 = Left InvalidBlockIndex
    | hashBlockHeader prevBlock.header /= block.header.previousHash      = Left InvalidPrevBlockHash
    | not (checkProofOfWork block.header)                                = Left InvalidBlockHash
    | block.header.merkleRoot /= createMerkleRoot block.transactions     = Left InvalidBlockMerkleRoot
    | otherwise = do

        validateBlockReward block.transactions block.header

        Traversable.traverse Tx.validateTransactionSig block.transactions
            # Bifunctor.bimap InvalidBlockTx (const unit)


checkProofOfWork :: BlockHeader -> Boolean
checkProofOfWork header =
    let
        headerHash = hashBlockHeader header
    in
    proofOfWorkMatch header.difficulty headerHash



genesisBlock :: Block
genesisBlock =
    let
        header =
            { previousHash: "0"
            , merkleRoot: ""
            , nonce: 0
            , difficulty: 0
            }

        headerHash = hashBlockHeader header
    in
    Block
        { index: 0
        , header: header
        , transactions: mempty
        }