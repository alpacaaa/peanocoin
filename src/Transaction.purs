module Peanocoin.Transaction where


import Prelude

import Crypto.Simple as Crypto
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, encodeJson, getField, toObject)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.String as String
import Peanocoin.Address (Address(..))
import Peanocoin.Hash (Hash)
import Peanocoin.Address as Address
import Peanocoin.Hash as Hash
import Peanocoin.Util as Util


type Transfer =
    { senderKey :: Crypto.PublicKey
    , recipient :: Address
    , amount    :: Int
    , timestamp :: Int
    }

type Reward =
    { minerKey :: Crypto.PublicKey -- ^ Public key of miner
    , reward   :: Int              -- ^ Decided by block difficulty
    }

data TransactionHeader
    = TransferHeader Transfer
    | RewardHeader Reward

data Transaction = Transaction
    { header    :: TransactionHeader
    , signature :: Crypto.Signature
    }


data InvalidTransaction
    = InvalidTransactionSignature
    | TxMustBeTransfer
    | TxAlreadyInMemPool
    | TxAlreadyInBlock Hash


instance encodeTxHeader :: EncodeJson TransactionHeader where
    encodeJson (TransferHeader { senderKey, recipient, amount, timestamp }) =
        (  "amount"    := amount
        ~> "senderKey" := Crypto.toString senderKey
        ~> "recipient" := Address.unwrap recipient
        ~> "timestamp" := timestamp
        ~> jsonEmptyObject
        )

    encodeJson (RewardHeader { minerKey, reward }) =
        (  "reward"   := reward
        ~> "minerKey" := Crypto.toString minerKey
        ~> jsonEmptyObject
        )


instance eqTransaction :: Eq Transaction where
    eq a b = (hashTransaction a) == (hashTransaction b)


instance showTx :: Show Transaction where
    show tx@(Transaction { header }) =
        [ "Transaction <type: "
        , txTypeToString tx
        , ", amount: "
        , show (txAmount tx)
        , ", hash: "
        , hashTransaction tx
        ]
        # String.joinWith ""


instance encodeTx :: EncodeJson Transaction where
    encodeJson tx@(Transaction { header, signature }) =
        (  "hash"      := hashTransaction tx
        ~> "signature" := Crypto.toString signature
        ~> "type"      := txTypeToString tx
        ~> "header"    := encodeJson header
        ~> jsonEmptyObject
        )


instance decodeTx :: DecodeJson Transaction where
    decodeJson json = do
        obj <- Either.note "Unable to parse" $ toObject json
        sig <- getField obj "signature"
        sig' <- Either.note "Invalid signature" (Util.bufferFromStringUnsafe sig)
        signature <- Either.note "Invalid signature" (Crypto.importFromBuffer sig')

        t      <- getField obj "type"
        header <- getField obj "header" >>= decodeTxHeader t

        pure $ Transaction { header, signature }

decodeTxHeader :: String -> StrMap Json -> Either String TransactionHeader
decodeTxHeader t obj = do
    case t of
        "transfer" -> decodeTxHeaderTransfer obj
        "reward"   -> decodeTxHeaderReward  obj
        _          -> Left "Invalid transaction type"


decodeTxHeaderTransfer :: StrMap Json -> Either String TransactionHeader
decodeTxHeaderTransfer obj = do
    keyBuffer <- getField obj "senderKey"
    key       <- Either.note "Invalid key" (Util.bufferFromStringUnsafe keyBuffer)

    recipient <- getField obj "recipient"
    pk        <- Either.note "Invalid key" (Crypto.importFromBuffer key)

    amount    <- getField obj "amount"
    timestamp <- getField obj "timestamp"

    pure $ TransferHeader
        { senderKey: pk
        , amount
        , recipient: Address recipient
        , timestamp
        }


decodeTxHeaderReward :: StrMap Json -> Either String TransactionHeader
decodeTxHeaderReward obj = do
    keyBuffer <- getField obj "minerKey"
    key       <- Either.note "Invalid key" (Util.bufferFromStringUnsafe keyBuffer)
    pk        <- Either.note "Invalid key" (Crypto.importFromBuffer key)

    reward    <- getField obj "reward"

    pure $ RewardHeader
        { minerKey: pk
        , reward
        }


txTypeToString :: Transaction -> String
txTypeToString tx =
    if isRewardTx tx then "reward" else "transfer"

txAmount :: Transaction -> Int
txAmount (Transaction { header }) =
    case header of
        RewardHeader   { reward } -> reward
        TransferHeader { amount } -> amount


hashTransactionHeader :: TransactionHeader -> Crypto.Digest
hashTransactionHeader (TransferHeader header) =
    let
        Address recipient = header.recipient
        blob =
            recipient
            <> Crypto.toString header.senderKey
            <> show header.amount
            <> show header.timestamp
    in
    Hash.hash blob

hashTransactionHeader (RewardHeader header) =
    let
        minerKey = Crypto.toString header.minerKey
        blob     = minerKey <> show header.reward
    in
    Hash.hash blob


hashTransaction' :: Transaction -> Crypto.Digest
hashTransaction' (Transaction transaction) =
    hashTransactionHeader transaction.header


hashTransaction :: Transaction -> String
hashTransaction =
    Crypto.toString <<< hashTransaction'

txMatchHash :: Hash -> Transaction -> Boolean
txMatchHash hash tx =
    hashTransaction tx == hash


transactionPublicKey :: Transaction -> Crypto.PublicKey
transactionPublicKey (Transaction tx) =
    case tx.header of
        TransferHeader header -> header.senderKey
        RewardHeader   header -> header.minerKey

validateTransactionSig :: Transaction -> Either InvalidTransaction Unit
validateTransactionSig tx =
    let
        publicKey  =
            transactionPublicKey tx

        headerHash =
            hashTransaction' tx

        Transaction { signature } =
            tx
    in
    if Crypto.verify publicKey signature headerHash
        then pure unit
        else Left InvalidTransactionSignature



isRewardTx :: Transaction -> Boolean
isRewardTx (Transaction tx) =
    case tx.header of
        RewardHeader _ -> true
        _              -> false



createTransaction :: Crypto.PrivateKey -> TransactionHeader -> Maybe Transaction
createTransaction private header =
    let
        headerHash =
            hashTransactionHeader header

        create signature =
            Transaction { header, signature }
    in
    map create (Crypto.sign private headerHash)
