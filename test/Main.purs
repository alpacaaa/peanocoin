module Test.Main where

import Prelude
import Peanocoin.Node (State(..), Event(..))
import Peanocoin.Block (Block(..))
import Peanocoin.Blockchain (Blockchain(..))
import Peanocoin.Ledger (TransferError(..))
import Peanocoin.Transaction (Transaction(..), TransactionHeader(..))

import Peanocoin.Block as Block
import Peanocoin.Blockchain as Blockchain
import Peanocoin.Transaction as Tx
import Peanocoin.Address as Address
import Peanocoin.Node as Node
import Peanocoin.Ledger as Ledger


import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypto.Simple as Crypto
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either (Either(..), fromRight, isRight)
import Data.Maybe  (Maybe, fromJust)
import Data.Int as Int
import Data.Monoid (mempty)
import Data.Set as Set
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Traversable (for)
-- import Debug.Trace as Debug
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)


type SampleState =
    { state :: Node.State
    , pairs :: Array Crypto.KeyPair
    }

type TestEffect =
    Aff (RunnerEffects ()) Unit

assert :: Boolean -> TestEffect
assert value =
    shouldEqual value true

try :: forall a. Maybe a -> a
try a =
    unsafePartial $ fromJust a

tryE :: forall a b. Either a b -> b
tryE a =
    unsafePartial $ fromRight a

sampleBlock :: Array Transaction -> Crypto.KeyPair -> Block -> Block
sampleBlock txs pair prevBlock =
    try $ Block.mineBlock txs pair prevBlock

sampleTx :: { from :: Crypto.KeyPair, to :: Crypto.KeyPair } -> Int -> Transaction
sampleTx { from, to } amount =
    let
        Seconds ts = convertDuration $ Instant.unInstant (unsafePerformEff Now.now)

        header = TransferHeader
                { senderKey : from.public
                , recipient : try (Address.pkToAddress to.public)
                , amount    : amount
                , timestamp : Int.floor ts
                }
    in
    try $ Tx.createTransaction from.private header

sampleChain :: Array Crypto.KeyPair -> Blockchain
sampleChain pairs =
    let
        build acc pair =
            let
                prev = try $ Array.last acc
                new  = sampleBlock [] pair prev
            in
            Array.snoc acc new
    in
    Array.foldl build [Block.genesisBlock 1] pairs


sampleState :: Eff (RunnerEffects ()) SampleState
sampleState = do
    a <- Crypto.generateKeyPair
    b <- Crypto.generateKeyPair
    c <- Crypto.generateKeyPair

    let pairs = [a,b,c]

    let state = State
                    { keyPair:    a
                    , host:       "localhost:3000"
                    , name:       "test-node"
                    , peers:      Set.fromFoldable ["a", "b", "c"]
                    , blockchain: sampleChain pairs
                    , memPool:    mempty
                    }

    pure { state, pairs }


topBlock :: State -> Block
topBlock (State state) =
    try $ Array.last state.blockchain


balanceTests :: TestEffect
balanceTests = do
    pair     <- liftEff Crypto.generateKeyPair
    receiver <- liftEff Crypto.generateKeyPair
    another  <- liftEff Crypto.generateKeyPair

    let
        pk1 = try $ Address.pkToAddress pair.public
        pk2 = try $ Address.pkToAddress receiver.public
        pk3 = try $ Address.pkToAddress another.public

        ledger = Ledger.transferAmount pk1 100 mempty
        l1     = tryE $ Ledger.updateBalance pk1 pk2 50 ledger

    try (Ledger.currentBalance pk1 l1) `shouldEqual` 50

    Ledger.updateBalance pk1 pk2 150 ledger
        `shouldEqual`
        Left (InsufficientBalance pk1 100)

    Ledger.updateBalance pk3 pk2 50 ledger
        `shouldEqual`
        Left (AddressDoesNotExist pk3)

    pure unit


ledgerTests :: TestEffect
ledgerTests = do
    { state: State { blockchain }, pairs } <- liftEff sampleState

    let ledger = tryE $ Ledger.buildLedger blockchain

    _ <- for pairs \pair -> do
        let
            pk =
                try $ Address.pkToAddress pair.public

            balance =
                try $ Ledger.currentBalance pk ledger

        balance `shouldEqual` 100

    pure unit


nextBlockTests :: TestEffect
nextBlockTests = do
    { state: State { blockchain } } <- liftEff sampleState

    let
        middle =
            try $ Array.index blockchain 1

        next   =
            try $ Array.index blockchain 2

        after  =
            tryE $ Blockchain.findBlockAfter blockchain (Block.hashBlock middle)

    Block.hashBlock next `shouldEqual` Block.hashBlock after

    pure unit


newPeerTests :: TestEffect
newPeerTests = do
    -- Starts with 3 nodes
    -- Add two, one is already known
    -- End up with 4 nodes

    { state } <- liftEff sampleState
    let
        ips = ["b", "d"]

        { state: State { peers } } =
            tryE $ Node.updateState state (ReceivePeers ips)

    Set.size peers       `shouldEqual` 4
    assert (Set.member "b" peers)
    pure unit


newBlockTests :: TestEffect
newBlockTests = do
    { state, pairs } <- liftEff sampleState
    let
        miner =
            try $ Array.head pairs

        block =
            sampleBlock [] miner (topBlock state)

        { state: State { blockchain } } =
            tryE $ Node.updateState state (ReceiveBlock block)

    Array.length blockchain `shouldEqual` 5

    let top =
            Block.hashBlock $ try (Array.last blockchain)
    top `shouldEqual` (Block.hashBlock block)

    let
        last =
            try (Array.index blockchain 3)
    
        validation =
            Block.validateBlock { prev: last, current: block }

    assert (isRight validation)

    pure unit


newTxTests :: TestEffect
newTxTests = do
    { state, pairs } <- liftEff sampleState

    let
        from = try $ Array.index pairs 0

        to   =
            try $ Array.index pairs 1

        tx   =
            sampleTx { from, to } 100

        { state: State { memPool } } =
            tryE $ Node.updateState state (ReceiveTx tx)

    Array.length memPool `shouldEqual` 1

    pure unit

invalidBlockTests :: TestEffect
invalidBlockTests = do
    { state, pairs } <- liftEff sampleState
    let miner = try $ Array.head pairs

    -- force invalid block
    let
        block =
            sampleBlock [] miner (topBlock state)
            # sampleBlock [] miner

        result =
            Node.updateState state (ReceiveBlock block)

    case result of
        Right _  ->
            fail "Invalid blocks should be rejected"
        Left err ->
            pure unit

    pure unit



miningTests :: TestEffect
miningTests = do
    { state, pairs } <- liftEff sampleState

    let
        from =
            try $ Array.index pairs 0

        to   =
            try $ Array.index pairs 1

        tx   =
            sampleTx { from, to } 20

        block =
            sampleBlock [tx] from (topBlock state)

        { state: State { blockchain } } =
            tryE $ Node.updateState state (ReceiveBlock block)

    Array.length blockchain `shouldEqual` 5

    let
        ledger =
            tryE $ Ledger.buildLedger blockchain

        balanceSender =
            try $ Ledger.currentBalance (try $ Address.pkToAddress from.public) ledger

        balanceReceiver =
            try $ Ledger.currentBalance (try $ Address.pkToAddress to.public) ledger

    balanceSender   `shouldEqual` 180
    balanceReceiver `shouldEqual` 120

    pure unit


encodeDecode :: forall a. (EncodeJson a)
             => (DecodeJson a)
             => (Eq a)
             => a
             -> Boolean
encodeDecode thing =
    let
        encoded  = stringify (encodeJson thing)
        decoded  = tryE (jsonParser encoded)
        reverted = tryE (decodeJson decoded)
    in
    reverted == thing



jsonTests :: TestEffect
jsonTests = do
    { state, pairs } <- liftEff sampleState
    let State { blockchain } = state

    let
        from =
            try $ Array.index pairs 0

        to   =
            try $ Array.index pairs 1

        tx   =
            sampleTx { from, to } 20

        last =
            try $ Array.index blockchain 1

        block =
            try $ Array.index blockchain 2

    assert (encodeDecode tx)
    assert (encodeDecode block)
    assert (encodeDecode blockchain)

    pure unit




main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    describe "Peano coin" do
        it "balance" balanceTests
        it "ledger"  ledgerTests
        it "next block" nextBlockTests
        it "handles new peers" newPeerTests
        it "handles new blocks" newBlockTests
        it "handles new transactions" newTxTests
        it "rejects invalid blocks" invalidBlockTests
        it "mines new blocks" miningTests
        it "encodes as json" jsonTests

