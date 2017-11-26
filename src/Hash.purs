module Peanocoin.Hash where

import Crypto.Simple as Crypto

type Hash = String

hash :: forall a. Crypto.Hashable a
     => a
     -> Crypto.Digest
hash = Crypto.hash Crypto.SHA256
