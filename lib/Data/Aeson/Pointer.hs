{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

-- | Description: JSON Pointers as described in RFC 6901.
module Data.Aeson.Pointer (
  Pointer(..),
  Key(..),
  Path,
  -- * Representing pointers
  formatPointer,
  parsePointer,
  -- * Using pointers
  get,
  pointerFailure
) where

import           Data.Aeson (encode)
import           Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as HM
import           Data.Aeson.Types (FromJSON, Parser, Result(Error), ToJSON, Value(Array, Object))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isNumber)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Autodocodec
import Autodocodec (Autodocodec)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Coerce (coerce)
import Data.Aeson.Pointer.ArrayOffset (ArrayOffset (ArrayOffset), (!?-))

-- * Patch components

-- | Path components to traverse a single layer of a JSON document.
data Key
    = OKey Aeson.Key -- ^ Traverse a 'Value' with an 'Object' constructor.
    | AKey ArrayOffset                -- ^ Traverse a 'Value' with an 'Array' constructor.
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON) via (Autodocodec Key)

instance Autodocodec.HasCodec Key where
  codec = Autodocodec.dimapCodec eitherToKey keyToEither eitherKeyCodec where
    eitherToKey :: Either Aeson.Key ArrayOffset -> Key
    eitherToKey = \case
      Left k -> OKey k
      Right i -> AKey i
    keyToEither :: Key -> Either Aeson.Key ArrayOffset
    keyToEither = \case
      OKey k -> Left k
      AKey i -> Right i
    eitherKeyCodec :: Autodocodec.JSONCodec (Either Aeson.Key ArrayOffset)
    eitherKeyCodec = Autodocodec.eitherCodec aesonKeyCodec arrayCodec
    aesonKeyCodec :: Autodocodec.JSONCodec Aeson.Key
    aesonKeyCodec = Autodocodec.dimapCodec AesonKey.fromText AesonKey.toText Autodocodec.codec
    arrayCodec :: Autodocodec.JSONCodec ArrayOffset
    arrayCodec = coerce (Autodocodec.boundedIntegralCodec :: Autodocodec.JSONCodec Int)

formatKey :: Key -> Text
-- todo: This is wrong as it doesn't handle negative keys
formatKey (AKey (ArrayOffset i)) = case i == -1 of 
  True -> "-"
  False -> T.pack (show i)
formatKey (OKey t) = T.concatMap esc $ toText t
  where
    esc :: Char -> Text
    esc '~' = "~0"
    esc '/' = "~1"
    esc c = T.singleton c

-- * Pointers

-- | A sequence of 'Key's forms a path through a JSON document.
type Path = [Key]

-- | Pointer to a location in a JSON document.
--
-- Defined in RFC 6901 <http://tools.ietf.org/html/rfc6901>
newtype Pointer = Pointer { pointerPath :: Path }
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid)
  deriving (ToJSON, FromJSON) via (Autodocodec Pointer)

-- | Format a 'Pointer' as described in RFC 6901.
--
-- >>> formatPointer (Pointer [])
-- ""
-- >>> formatPointer (Pointer [OKey ""])
-- "/"
-- >>> formatPointer (Pointer [OKey " "])
-- "/ "
-- >>> formatPointer (Pointer [OKey "foo"])
-- "/foo"
-- >>> formatPointer (Pointer [OKey "foo", AKey (ArrayOffset 0)])
-- "/foo/0"
-- >>> formatPointer (Pointer [OKey "a/b"])
-- "/a~1b"
-- >>> formatPointer (Pointer [OKey "c%d"])
-- "/c%d"
-- >>> formatPointer (Pointer [OKey "e^f"])
-- "/e^f"
-- >>> formatPointer (Pointer [OKey "g|h"])
-- "/g|h"
-- >>> formatPointer (Pointer [OKey "i\\j"])
-- "/i\\j"
-- >>> formatPointer (Pointer [OKey "k\"l"])
-- "/k\"l"
-- >>> formatPointer (Pointer [OKey "m~n"])
-- "/m~0n"
-- >>> formatPointer (Pointer [OKey "foo", AKey (ArrayOffset (-1))])
-- "/foo/-"
formatPointer :: Pointer -> Text
formatPointer (Pointer []) = ""
formatPointer (Pointer path) = "/" <> T.intercalate "/" (formatKey <$> path)

-- | Parse a 'Pointer' as described in RFC 6901.
parsePointer :: Text -> Parser Pointer
parsePointer t
  | T.null t = return (Pointer [])
  | otherwise = Pointer <$> mapM key (drop 1 $ T.splitOn "/" t)
  where
    step t
      | "0" `T.isPrefixOf` t = T.cons '~' (T.tail t)
      | "1" `T.isPrefixOf` t = T.cons '/' (T.tail t)
      | otherwise = T.cons '~' t
    unesc :: Text -> Text
    unesc t =
      let l = T.split (== '~') t
      in T.concat $ take 1 l <> fmap step (tail l)
    key t
      | T.null t         = fail "JSON components must not be empty."
      -- todo: Although this is technically safe here as `isNumber` should prevent any failures of `read`,
      -- probably better not to use `read` here and instead "parse, don't validate".
      | T.all isNumber t = return (AKey (ArrayOffset (read $ T.unpack t)))
      | otherwise        = pure $ case t == "-" of
          True -> AKey (ArrayOffset (-1))
          False -> OKey . fromText $ unesc t

instance Autodocodec.HasCodec Pointer where
  codec = Autodocodec.bimapCodec (Aeson.parseEither parsePointer) formatPointer Autodocodec.codec

-- | Follow a 'Pointer' through a JSON document as described in RFC 6901.
get :: Pointer -> Value -> Result Value
get (Pointer pointer) v = case pointer of
  [] -> return v
  (key : rest) -> case key of
    OKey k -> case v of
      Object o -> maybe (fail "") (get (Pointer rest)) (HM.lookup k o)
      _ -> pointerFailure (Pointer pointer) v
    AKey i -> case v of
      Array a -> maybe (fail "") (get (Pointer rest)) (a !?- i)
      _ -> pointerFailure (Pointer pointer) v


-- | Report an error while following a pointer.
pointerFailure :: Pointer -> Value -> Result a
pointerFailure (Pointer []) _value = Error "Cannot follow empty pointer. This is impossible."
pointerFailure (Pointer path@(key:_)) value =
    Error . BS.unpack $ "Cannot follow pointer " <> pt <> ". Expected " <> ty <> " but got " <> doc
  where
    doc = encode value
    pt = encode path
    ty = case key of
           (AKey _) -> "array"
           (OKey _) -> "object"


-- $setup
-- >>> :set -XOverloadedStrings
