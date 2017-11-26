module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Crypto.Simple as Crypto
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid (mempty)
import Node.Yargs.Setup       as Yargs
import Node.Yargs.Applicative as Yargs

import Peanocoin.Block as Block
import Peanocoin.Node as Node
import Peanocoin.Server as Server


setup :: Yargs.YargsSetup
setup =
    Yargs.string "port"


buildOptions :: String -> Eff Server.AppEffects { port :: Int }
buildOptions port =
    let
        p = Int.fromString port
            # Maybe.maybe 3000 id
    in
    pure { port: p }



main :: Eff Server.AppEffects Unit
main = do
    args <-  Yargs.runY setup $ (pure buildOptions)
         <*> Yargs.yarg "p" ["port"] (Just "Server port") (Left "3000") false

    pair <- Crypto.generateKeyPair

    let
        port =
            args.port

        host =
            "http://localhost:" <> (show port)

        initialState
            = Node.State
            { keyPair:    pair
            , host:       host
            , name:       (show port)
            , peers:      mempty
            , blockchain: [Block.genesisBlock]
            , memPool:    mempty
            }

    void $ Server.main port initialState
