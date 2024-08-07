-- | Description: Two versions of JSON Pointers:
-- 
-- 1. As described in RFC 6901.
-- 2. As an array of path elements.
--
-- The RFC 6901 format, whilst visually a nicer human readable format (as it looks like a standard directory string)
-- is more tricky to work with programmatically, as generating paths requires concatenating strings and escaping
-- special characters. 
--
-- Moreover, the RFC 6901 escaping rules are unique to the RFC 6901 format. 
--
-- As an alternative to this format, we also provide a format for pointers that is simply a JSON array of path elements.
--
-- This format is easier to work with programmatically, as escaping is no longer required, 
-- and will generate Swagger/OpenAPI indicating that an array of path elements is required. 
-- The only escaping required is standard JSON escaping. 
module Data.Aeson.Pointer (
  Pointer(..),
  Key(..),
  Path,
  -- * Representing pointers
  formatPointer,
  parsePointer,
  -- * Using pointers
  get,
  pointerFailure,
  Pointer'(..),
  ExpectedFormat(..),
  ParsingStrictness(..),
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
import Data.Aeson.Pointer.ArrayOffset (ArrayOffset (ArrayOffset))
import Data.Aeson.Pointer.ArrayOffset.Utilities ((!?-))

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
newtype Pointer = Pointer { pointerPath :: Path }
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid)
  deriving (ToJSON, FromJSON) via (Autodocodec StandardPointer)

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

--

-- | Determines how we format the pointer. 
-- 
-- - `RFC6901Format` to format the pointer as described in RFC 6901 <http://tools.ietf.org/html/rfc6901>
-- - `ArrayFormat` to format the pointer as a JSON array of path elements.
type data ExpectedFormat = RFC6901Format | ArrayFormat

-- | Determines how strictly we parse the pointer based on the expected format.
-- 
-- - `StrictParsing` to parse the pointer strictly according to the expected format.
-- - `LenientParsing` to parse the pointer leniently, allowing for the pointer to be in either format on input.
--   In this case, the pointer will still be output as whatever the `ExpectedFormat` is, we don't retain the input format.
--   Note that whenever `LenientParsing` is used, the Swagger/OpenAPI schema will indicate that a pointer is either
--   a string or an array of path elements (which themselves are strings or ints). 
--   So this will presumably generate sumtypes in whatever code is generated,
--   even though in actuality the pointer will always be formatted as specified by `ExpectedFormat`.
--   As a result it may be better when exposing an API to use `StrictParsing` for "output" parameters
--   to more accurately reflect the the format you are exposing to clients.
type data ParsingStrictness = StrictParsing | LenientParsing

type StandardPointer = Pointer' RFC6901Format StrictParsing

-- | A newtype wrapper around a `Pointer`. 
-- 
-- The parameters will affect how the pointer is serialised/deserialised.
-- As the type paramters are phantom, one can `coerce` between different `Pointer'` types, 
-- and indeed from `Pointer'` to `Pointer` and back again for free.
--
-- Also note that, whilst RFC 6901 allows one to specify the end of an array using @"-"@, the array format does not treat @"-"@ speciallly.
-- Instead, it path elements of JSON type "numeric" as array indicies, and all strings as object keys. 
-- It also follows the common convention of treating negative indicies as "from the end of the array".
--
-- So the path @"/foo/-"@ in RFC 6901 format would be @["foo", -1]@ in array format.
--
-- Important to note that array format also allows for paths like @["foo", -3]@, indicating the third last element of the array.
-- RFC 6901 does not have a way of representing this. 
--
-- The `FromJSON` methods for `Pointer' RFC6901Format StrictParsing` will not accept negative indicies as per the RFC, 
-- and the `ToJSON` methods of `Pointer'` for RFC 6901 will replace @-1@ with @"-"@, as per RFC 6901.
-- But the `ToJSON` method of `Pointer' RFC6901Format` will output negative indicies other that @-1@ literally, 
-- for example @["foo", -3]@ will be output as @"/foo/-3"@. 
--
-- This is technically not RFC 6901 compliant, but you'll only get this issue if you deserialise a pointer using the array format
-- and then serialise it using the RFC 6901 format.
newtype Pointer' (expectedFormat :: ExpectedFormat) (parsingStrictness :: ParsingStrictness) = Pointer' Pointer

instance Autodocodec.HasCodec (Pointer' RFC6901Format StrictParsing) where
  codec = coerce $ Autodocodec.bimapCodec rfc6901ToPointer formatPointer Autodocodec.codec 

deriving via Path instance Autodocodec.HasCodec (Pointer' ArrayFormat StrictParsing)

instance Autodocodec.HasCodec (Pointer' RFC6901Format LenientParsing) where
  codec = coerce $ Autodocodec.bimapCodec textOrPathToPointer (Left . formatPointer) baseLenientCodec

instance Autodocodec.HasCodec (Pointer' ArrayFormat LenientParsing) where
  codec = coerce $ Autodocodec.bimapCodec textOrPathToPointer pointerToPath baseLenientCodec where
    pointerToPath :: Pointer -> Either Text Path
    pointerToPath = Right . pointerPath

baseLenientCodec :: Autodocodec.JSONCodec (Either Text Path)
baseLenientCodec = Autodocodec.disjointEitherCodec Autodocodec.codec Autodocodec.codec

rfc6901ToPointer :: Text -> Either String Pointer
rfc6901ToPointer = Aeson.parseEither parsePointer

textOrPathToPointer :: Either Text Path -> Either String Pointer
textOrPathToPointer = either rfc6901ToPointer (Right . Pointer)

-- $setup
-- >>> :set -XOverloadedStrings
