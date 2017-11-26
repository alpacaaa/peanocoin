module Peanocoin.Util where

import Prelude
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Unsafe as Unsafe
import Node.Buffer as Buffer
import Node.Encoding as Encoding

import Data.Either as Either
import Data.Maybe (Maybe(..))


bufferFromStringUnsafe :: String -> Maybe Buffer.Buffer
bufferFromStringUnsafe s =
    let
        eff = Buffer.fromString s Encoding.Hex
            # Exception.try
            # Unsafe.unsafePerformEff
    in
    Either.either (const Nothing) Just eff
