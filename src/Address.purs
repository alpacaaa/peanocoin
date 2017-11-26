module Peanocoin.Address where

import Prelude
import Data.Maybe (Maybe)
import Data.Argonaut (class EncodeJson, fromString)

import Crypto.Simple as Crypto

newtype Address = Address String

derive instance eqAddress :: Eq Address
derive instance ordAddress :: Ord Address


instance showAddress :: Show Address where
    show (Address str) = str

instance encodeJsonAddress :: EncodeJson Address where
    encodeJson (Address str) = fromString str


unwrap :: Address -> String
unwrap (Address a) = a

pkToAddress :: Crypto.PublicKey -> Maybe Address
pkToAddress key = map Address (encodeAddress key)

encodeAddress :: Crypto.PublicKey -> Maybe String
encodeAddress pk = pk
    # Crypto.hash Crypto.SHA256
    # Crypto.hash Crypto.SHA256
    # Crypto.hash Crypto.RIPEMD160
    # Crypto.toString
    # Crypto.baseEncode Crypto.BASE58
    # map Crypto.toString
