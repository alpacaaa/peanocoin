module Peanocoin.Server where

import Peanocoin.Node (State(..), Event(..), Effect(..), Error(..), Peer)
import Peanocoin.Address (Address(..))
import Peanocoin.Block (InvalidBlock(..))
import Peanocoin.Blockchain (Blockchain(..), FindBlockError(..))
import Peanocoin.Ledger (TransferError(..))
import Peanocoin.MemPool (MemPool(..))
import Peanocoin.Transaction (Transaction(..), TransactionHeader(..), InvalidTransaction(..))

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Ref as Ref
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Argonaut
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (foldM)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap as StrMap
import Data.Time.Duration (Seconds(..))
import Data.Time.Duration as Duration
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax as Affjax
import Network.HTTP.RequestHeader (RequestHeader(..))
import Node.Express.App as Express
import Node.Express.Handler as Express
import Node.Express.Request (getOriginalUrl, getQueryParam, getRequestHeader, getRouteParam)
import Node.Express.Response as Express
import Node.Express.Types as Express
import Node.HTTP as Http

import Peanocoin.Hash (Hash)
import Peanocoin.Address as Address
import Peanocoin.Block as Block
import Peanocoin.Blockchain as Blockchain
import Peanocoin.Ledger as Ledger
import Peanocoin.MemPool as MemPool
import Peanocoin.Node as Node
import Peanocoin.Transaction as Tx


type AppEffects = 
    ( ajax :: Affjax.AJAX
    , console :: Console.CONSOLE
    , ref :: Ref.REF
    , express :: Express.EXPRESS
    , now :: Now.NOW
    , exception :: Exception.EXCEPTION
    )

type StateRef =
    Ref.Ref State

type AppHandler =
    Express.HandlerM AppEffects Unit

data Announcement
    = AnnounceBlock
    | AnnounceTransaction

decodeJson :: forall a. DecodeJson a => String -> Either String a
decodeJson json = do
    parsed <- Argonaut.jsonParser json
    Argonaut.decodeJson parsed

peersToArray :: Set Peer -> Array Peer
peersToArray peers = Set.toUnfoldable peers

logInfo :: forall m. (MonadAff AppEffects m) => String -> m Unit
logInfo = liftEff <<< Console.log

logError :: String -> AppHandler
logError error = do
    liftEff $ Console.log error
    Express.sendJson error


describeEvent :: Event -> String
describeEvent = case _ of
    ReceiveBlock block ->
        "Block " <> (Block.hashBlock block) <> " is valid and added to the blockchain"

    ReceiveTx tx ->
        "Transaction " <> (Tx.hashTransaction tx) <> "is valid and added to the mempool"

    ReceivePeers peers ->
        (show $ Array.length peers) <> " peers added to the list of known peers"


describeError :: Error -> String
describeError = case _ of
    ErrorBlock err ->
        describeInvalidBlockError err
    ErrorTx err ->
        describeInvalidTxError err
    ErrorBlockNotFound err ->
        describeFindBlockError err


describeTransferError :: TransferError -> String
describeTransferError = case _ of
    AddressDoesNotExist (Address address) ->
        "Address does not exist: " <> address
    InvalidPublicKey ->
        "Invalid Public Key"
    InvalidTransferAmount amount ->
        "Transfer amount " <> (show amount) <> " is invalid"
    InsufficientBalance (Address address) balance ->
        "Current balance of address " <> address <> "is: " <> (show balance)
        <> " which is insufficient for the transfer requested"


describeFindBlockError :: FindBlockError -> String
describeFindBlockError = case _ of
    FindBlockInvalidHash hash ->
        "Block not found with this hash: " <> hash
    FindBlockNoMoreBlocks ->
        "There are no more blocks!"



describeInvalidBlockError :: InvalidBlock -> String
describeInvalidBlockError = case _ of
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
        describeInvalidTxError err
    InvalidEmptyBlockchain ->
        "Blockchain is empty"


describeInvalidTxError :: InvalidTransaction -> String
describeInvalidTxError = case _ of
    InvalidTransactionSignature ->
        "Transaction signature is invalid"
    TxMustBeTransfer ->
        "Only transactions of type Transfer are accepted"
    TxAlreadyInMemPool ->
        "Transaction is already known and in the mempool"
    TxAlreadyInBlock hash ->
        "Transaction is already known and mined in block: " <> hash


indexHandler :: StateRef -> AppHandler
indexHandler stateRef = do
    State { keyPair, blockchain, memPool, nodes } <- liftEff $ Ref.readRef stateRef
    let
        address =
            Address.pkToAddress keyPair.public
                # Maybe.maybe "error computing address" Address.unwrap

        peers = peersToArray nodes

        body =
            { address:    address
            , blockchain: Argonaut.encodeJson blockchain
            , memPool:    Argonaut.encodeJson memPool
            , peers:      Argonaut.encodeJson peers
            }

    Express.sendJson body


handleUpdate :: forall a. Either Error a -> AppHandler
handleUpdate result =
    case result of
        Right _ -> do
            Express.sendJson "ok"
        
        Left err -> do
            let msg = describeError err
            logError msg
            Express.sendJson msg


process :: StateRef -> Event -> AppHandler
process stateRef event = do
    result <- process' stateRef event
    handleUpdate result


process' :: forall m. (MonadAff AppEffects m)
         => StateRef
         -> Event
         -> m (Either Error (Array Event))
process' stateRef event = do
    logInfo (describeEvent event)

    current <- liftEff $ Ref.readRef stateRef
    let result = Node.updateState current event

    case result of
        Right { state, effect } -> do
            liftEff $ Ref.writeRef stateRef state
            events <- liftAff $ processEffects state effect
            pure (Right events)

        Left err -> pure (Left err)



fetchNextBlock :: String
               -> (Array Event)
               -> Peer
               -> Aff AppEffects (Array Event)
fetchNextBlock hash result peer = do
    -- is this even correct?
    when (Array.length result > 0) $ pure unit

    { response } <- Affjax.get (peer <> "/block-after/" <> hash)

    case decodeJson response of
        Right block -> do
            logInfo "Received the block we requested"
            pure [ReceiveBlock block]
        Left _      -> pure []


processEffects :: State
               -> Effect
               -> Aff AppEffects (Array Event)
processEffects (State state) = case _ of
    SendBlock block -> do
        let
            hash   = Block.hashBlock block
            path   = "/announce/block/" <> hash
            origin = state.host <> "/block/" <> hash

        _ <- liftAff $ broadcast origin path state.nodes

        -- Make sure we have the most recent version of the blockchain
        foldM (fetchNextBlock hash) mempty state.nodes

    SendTx tx -> do
        let
            hash   = Tx.hashTransaction tx
            path   = "/announce/transaction/" <> hash
            origin = state.host <> "/transaction/" <> hash

        _ <- liftAff $ broadcast origin path state.nodes
        pure []

    RequestPeers peer -> do
        let
            config  = { origin: state.host, destination: peer }
            request = prepareRequest config "/peers"

        { response } <- Affjax.affjax request

        case decodeJson response of
            Right peers -> pure [ReceivePeers peers]
            Left error  -> do
                liftEff $ Console.log (show error)
                pure []

    RequestBlockAfter hash -> do
        liftEff $ Console.log ("Requesting block after " <> hash)
        foldM (fetchNextBlock hash) mempty state.nodes

    NoEffect -> pure []



ignoreResponse :: Affjax.AffjaxRequest Json
               -> Aff AppEffects (Affjax.AffjaxResponse Unit)
ignoreResponse request = Affjax.affjax request

broadcast :: Peer -> String -> Set Peer -> Aff AppEffects Unit
broadcast origin path peers =
    void $ traverse loop (peersToArray peers)

    where
        loop peer = do
            let
                config  = { origin: origin, destination: peer }
                request = prepareRequest config path

            Aff.apathize (ignoreResponse request)


applyEvents :: Ref.Ref State
            -> Array Event
            -> AppHandler
applyEvents stateRef events =
    void $ for events \ev -> do
        result <- process' stateRef ev
        case result of
            Right newEvents -> applyEvents stateRef newEvents
            Left  err       -> pure unit



peersHandler :: StateRef -> AppHandler
peersHandler stateRef = do
    origin <- getRequestHeader "x-origin"
    (State state) <- liftEff $ Ref.readRef stateRef

    -- Try to add the requesting peer to our list
    -- This should be moved out and executed on every request
    -- Not only that, if it's a new peer it should ask for its list
    case origin of
        Just peer ->
            void $ process' stateRef (ReceivePeers [peer])
        Nothing   ->
            pure unit

    let peers = Array.cons state.host (peersToArray state.nodes)
    logInfo $ "Sending " <> (show $ Array.length peers) <> " peers"

    Express.sendJson $ Argonaut.encodeJson peers



blockHandler :: StateRef -> AppHandler
blockHandler stateRef = do
    hash' <- getRouteParam "hash"
    state <- liftEff $ Ref.readRef stateRef

    case hash' of
        Just hash ->
            let
                (State { blockchain }) = state
                result = Blockchain.findBlock blockchain hash
            in

            case result of
                Right block -> do
                    logInfo $ "Sending block " <> (show $ Block.hashBlock block)
                    Express.sendJson (Argonaut.encodeJson block)

                Left error  ->
                    logError "Block not found"
        
        Nothing ->
            logError "Invalid block hash"



txHandler :: StateRef -> AppHandler
txHandler stateRef = do
    hash' <- getRouteParam "hash"
    state <- liftEff $ Ref.readRef stateRef

    case hash' of
        Just hash ->
            let
                (State { blockchain, memPool }) = state
                result = findTx blockchain memPool hash
            in

            case result of
                Just tx -> do
                    logInfo $ "Sending transaction " <> (Tx.hashTransaction tx)
                    Express.sendJson (Argonaut.encodeJson tx)

                Nothing -> logError "Transaction not found"
        
        Nothing ->
            logError "Invalid block transaction"






blockAfterHandler :: StateRef -> AppHandler
blockAfterHandler stateRef = do
    hash' <- getRouteParam "hash"
    state <- liftEff $ Ref.readRef stateRef

    case hash' of
        Just hash ->
            let
                (State { blockchain }) = state
                result = Blockchain.findBlockAfter blockchain hash
            in

            case result of
                Right block -> do
                    logInfo $ "Sending next block: " <> (Block.hashBlock block)
                    Express.sendJson (Argonaut.encodeJson block)

                Left error  ->
                    logError (describeFindBlockError error)
        
        Nothing ->
            logError "Invalid block hash"



announcementForBlock :: StateRef -> Hash -> String -> AppHandler
announcementForBlock stateRef hash origin = do
    state <- liftEff $ Ref.readRef stateRef
    let
        State { blockchain } =
            state

        result =
            Blockchain.findBlock blockchain hash

    case result of
        Right block -> do
            logInfo "Block already in blockchain"
            Express.sendJson "ok"

        Left  _     -> do
            { response } <- liftAff (Affjax.get origin)
            let event = map ReceiveBlock (decodeJson response)
            Either.either logError (process stateRef) event


announcementForTx :: StateRef -> Hash -> String -> AppHandler
announcementForTx stateRef hash origin = do
    state <- liftEff $ Ref.readRef stateRef
    let
        State { blockchain, memPool } =
            state

        result =
            findTx blockchain memPool hash

    case result of
        Just _  -> do
            logInfo "Transaction already in blockchain or mempool"
            Express.sendJson "ok"

        Nothing -> do
            { response } <- liftAff (Affjax.get origin)

            let event = map ReceiveTx (decodeJson response)
            Either.either logError (process stateRef) event



announce :: Announcement -> StateRef -> AppHandler
announce what stateRef = do
    hash   <- getRouteParam    "hash"
    origin <- getRequestHeader "x-origin"

    let callback =
            case what of
                AnnounceBlock       -> announcementForBlock
                AnnounceTransaction -> announcementForTx

    pure (callback stateRef) <*> hash <*> origin
        # Maybe.maybe (logError "Invalid data at /announce") id



ledgerHandler :: StateRef -> AppHandler
ledgerHandler stateRef = do
    state <- liftEff $ Ref.readRef stateRef

    let State { blockchain } = state

    case Ledger.buildLedger blockchain of
        Right ledger -> do
            logInfo "Sending ledger"
            let
                unfolded =
                    Map.toUnfoldable ledger :: Array (Tuple Address Int)

                strmap   =
                    unfolded
                        # map (\(Tuple k v) -> Tuple (Address.unwrap k) v)
                        # StrMap.fromFoldable 

            Express.sendJson $ Argonaut.encodeJson strmap

        Left error -> logError (describeTransferError error)


transferHandler :: StateRef -> AppHandler
transferHandler stateRef = do
    address <- getRouteParam "address"
    amount' <- getRouteParam "amount"
    
    let amount = amount' >>= Int.fromString

    pure (transferHandler' stateRef) <*> address <*> amount
        # Maybe.maybe (logError "invalid transfer data") id


transferHandler' :: StateRef -> String -> Int -> AppHandler
transferHandler' stateRef address amount = do
    State state <- liftEff $ Ref.readRef stateRef
    now         <- liftEff Now.now

    let
        (Seconds ts) =
            Instant.unInstant now
            # Duration.convertDuration

        header = TransferHeader
            { senderKey : state.keyPair.public
            , recipient : Address address
            , amount    : amount
            , timestamp : Int.floor ts
            }

        tx' = Tx.createTransaction state.keyPair.private header

    case tx' of
        Just tx -> transferHandler'' stateRef state.blockchain tx
        Nothing -> logError "Unable to create transaction"



transferHandler'' :: StateRef -> Blockchain -> Transaction -> AppHandler
transferHandler'' stateRef blockchain tx = do
    case validateTx blockchain tx of
        Right _ -> do
            logInfo "Transaction for transfer created"
            process stateRef (ReceiveTx tx)

        Left err ->
            logError (describeTransferError err)


validateTx :: Blockchain -> Transaction -> Either Ledger.TransferError Unit
validateTx blockchain tx = do
    ledger <- Ledger.buildLedger blockchain
    _      <- Ledger.applyTransaction ledger tx -- make sure there are enough funds
    pure unit



mineHandler :: StateRef -> AppHandler
mineHandler stateRef = do
    State { keyPair, blockchain, memPool } <- liftEff $ Ref.readRef stateRef

    let receive block =
            process stateRef (ReceiveBlock block)

    case Array.last blockchain of
        Just prevBlock ->
            Block.mineBlock memPool keyPair prevBlock
                # Maybe.maybe (logError "Could not mine block") receive

        Nothing -> logError "Empty blockchain"




bootHandler :: StateRef -> AppHandler
bootHandler stateRef = do
    ip'   <- getQueryParam "ip"
    state <- liftEff $ Ref.readRef stateRef

    case ip' of
        Just ip -> do
            events <- liftAff $ processEffects state (RequestPeers ip)
            applyEvents stateRef events
            Express.sendJson "ok"

        Nothing ->
            logError "Invalid ip"


prepareRequest :: { origin :: Peer, destination :: Peer } 
               -> String
               -> Affjax.AffjaxRequest Json
prepareRequest { origin, destination } path =
    let
        headers = [RequestHeader "x-origin" origin]
        url     = destination <> path
    in
        { method : Left Method.GET
        , url    : url
        , headers: headers
        , content: Nothing
        , username: Nothing
        , password: Nothing
        , withCredentials: false
        }



findTx :: Blockchain -> MemPool -> Hash -> Maybe Transaction
findTx blockchain memPool hash =
    case Blockchain.txInBlockchain blockchain hash of
        Just { tx } -> Just tx
        Nothing     -> MemPool.find memPool hash





logger :: StateRef -> AppHandler
logger stateRef = do
    State state <- liftEff $ Ref.readRef stateRef
    url         <- getOriginalUrl

    let port = state.name
    liftEff $ Console.log ("[" <> port <> "]" <> url)
    Express.next



appSetup :: StateRef -> Express.AppM AppEffects Unit
appSetup stateRef = do
    liftEff $ Console.log "Setting up"

    Express.setProp "json spaces" 4.0
    Express.use (logger stateRef)

    Express.get "/"                             (indexHandler      stateRef)
    
    -- Public stuff -> called by other peers
    Express.get "/peers"                        (peersHandler      stateRef)
    Express.get "/block/:hash"                  (blockHandler      stateRef)
    Express.get "/transaction/:hash"            (txHandler         stateRef)
    Express.get "/block-after/:hash"            (blockAfterHandler stateRef)

    -- contain x-origin to fetch the whole content
    Express.get "/announce/block/:hash"         (announce AnnounceBlock       stateRef)
    Express.get "/announce/transaction/:hash"   (announce AnnounceTransaction stateRef)

    -- Private stuff -> called by client
    -- issue transfers from this node to another peer
    Express.get "/boot"                         (bootHandler     stateRef)
    Express.get "/ledger"                       (ledgerHandler   stateRef)
    Express.get "/transfer/:address/:amount"    (transferHandler stateRef)
    Express.get "/mine-block"                   (mineHandler     stateRef)

    -- useOnError (errorHandler stateRef)


main :: Int -> State -> Eff AppEffects Http.Server
main port state = do
    stateRef <- Ref.newRef state

    Express.listenHttp (appSetup stateRef) port \_ ->
        Console.log $ "Listening on " <> show port








