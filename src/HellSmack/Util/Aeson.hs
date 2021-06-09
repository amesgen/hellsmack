module HellSmack.Util.Aeson
  ( CustomJSONLabel,
    decodeJSON,
    decodeJSONValue,
    encodeJSON,

    -- * reexports
    module Reexport,
  )
where

import Data.Aeson as Reexport
import Data.Aeson.Lens as Reexport
import Data.Aeson.Types (parseEither)
import Deriving.Aeson as Reexport
import HellSmack.Util.Exception

type CustomJSONLabel ms = CustomJSON '[FieldLabelModifier ms]

decodeJSON :: forall a m. (MonadIO m, FromJSON a) => LByteString -> m a
decodeJSON = rethrow . eitherDecode'

decodeJSONValue :: forall a m. (MonadIO m, FromJSON a) => Value -> m a
decodeJSONValue = rethrow . parseEither parseJSON

encodeJSON :: ToJSON a => a -> LByteString
encodeJSON = encode
