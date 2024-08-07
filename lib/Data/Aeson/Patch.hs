{-# LANGUAGE UndecidableInstances #-}

-- | Description: Represent RFC 6902 patches.
module Data.Aeson.Patch (
  Patch(..),
  Patch'(..),
  Operation(..),
  Operation'(..),
  -- * Modification
  modifyPointer,
  modifyPointers,
  -- * Predicates
  isAdd,
  isRem,
  isRep,
  isMov,
  isCpy,
  isTst,
  patch,
  applyOperation
) where

import Data.Aeson (FromJSON, ToJSON, Result, Value)
import Data.Aeson.Pointer (Pointer (Pointer), ExpectedFormat (RFC6901Format, ArrayFormat), ParsingStrictness (StrictParsing, LenientParsing), Pointer'(..))
import qualified Autodocodec
import Autodocodec ((.=), Autodocodec)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.HashMap.Strict as HashMap
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.Foldable (foldlM)
import Data.Aeson.Patch.Utilities (applyAdd, applyRem, applyRep, applyTst, applyCpy, applyMov)
import Data.Monoid.Action (Action (act))
import Data.Monoid (Dual (getDual))
import Control.Arrow ((>>>))

-- * Patches

-- | Describes the changes between two JSON documents.
newtype Patch = Patch
    { patchOperations :: DList Operation }
  deriving stock Show
  deriving newtype (Eq, Semigroup, Monoid)
  deriving (ToJSON, FromJSON) via Autodocodec (Patch' RFC6901Format StrictParsing)

-- | A newtype wrapper for `Patch` which allows us to specify the expected format of the pointers in the patch.
--
-- See `Pointer'` for more information.
newtype Patch' (expectedFormat :: ExpectedFormat) (parsingStrictness :: ParsingStrictness) = Patch' Patch

deriving via Autodocodec (Patch' expectedFormat parsingStrictness) instance 
  Autodocodec.HasObjectCodec (Operation' expectedFormat parsingStrictness) => ToJSON (Patch' expectedFormat parsingStrictness)
deriving via Autodocodec (Patch' expectedFormat parsingStrictness) instance 
  Autodocodec.HasObjectCodec (Operation' expectedFormat parsingStrictness) => FromJSON (Patch' expectedFormat parsingStrictness)
deriving via DList (Operation' expectedFormat parsingStrictness) instance 
  Autodocodec.HasObjectCodec (Operation' expectedFormat parsingStrictness) => Autodocodec.HasCodec (Patch' expectedFormat parsingStrictness)

-- | Action defined on Patch
--
-- Note we need to define this on `Dual Patch` because the laws of the typeclass `Action` require that:
-- @(a <> b) `act` x = a `act` (b `act` x)@. 
-- So the "last" patch needs to be the leftmost. But the semigroup instance on `Patch` is defined to be right-biased.
-- Wrapping in `Dual` flips the semigroup operation. 
instance Action (Dual Patch) (Result Value) where
  act :: Dual Patch -> Result Value -> Result Value
  act = getDual >>> patch >>> (=<<) 

-- * Patching

-- | Apply a patch to a JSON document.
patch
    :: Patch
    -> Value
    -> Result Value
patch (Patch ops) val = foldlM (flip applyOperation) val ops

-- | Apply an 'Operation' to a 'Value'.
applyOperation
    :: Operation
    -> Value
    -> Result Value
applyOperation op json = case op of
    Add path v'   -> applyAdd path v' json
    Rem path      -> applyRem path    json
    Rep path v'   -> applyRep path v' json
    Tst path v    -> applyTst path v  json
    Cpy path from -> applyCpy path from json
    Mov path from -> applyMov path from json

-- | Modify the pointers in the 'Operation's of a 'Patch'.
--
-- See 'modifyPointer' for details.
modifyPointers :: (Pointer -> Pointer) -> Patch -> Patch
modifyPointers f (Patch ops) = Patch (modifyPointer f <$> ops)

-- * Operations

-- | An 'Operation' describes the operations which can appear as part of a JSON
-- Patch.
--
-- See RFC 6902 Section 4 <http://tools.ietf.org/html/rfc6902#section-4>.
data Operation
    = Add { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.1
    | Cpy { changePointer :: Pointer, fromPointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.5
    | Mov { changePointer :: Pointer, fromPointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.4
    | Rem { changePointer :: Pointer }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.2
    | Rep { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.3
    | Tst { changePointer :: Pointer, changeValue :: Value }
    -- ^ http://tools.ietf.org/html/rfc6902#section-4.6
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON) via Autodocodec (Operation' RFC6901Format StrictParsing)

-- | A newtype wrapper for `Operation` which allows us to specify the expected format of the pointers in the patch.
--
-- See `Pointer'` for more information.
newtype Operation' (expectedFormat :: ExpectedFormat) (parsingStrictness :: ParsingStrictness) = Operation' Operation

instance Autodocodec.HasObjectCodec (Operation' expectedFormat parsingStrictness) => Autodocodec.HasCodec (Operation' expectedFormat parsingStrictness) where
  codec = Autodocodec.object "JsonPatchOperation" Autodocodec.objectCodec

-- Here we explicitly define instances for all the possible combinations of expected format and parsing strictness.
-- We do this then this will compile to 4 separate codec objects, each of which will only be only computed once.
-- If we had a more general instance, like:
--
-- instance Autodocodec.HasCodec (Pointer' expectedFormat parsingStrictness) => 
--   Autodocodec.HasObjectCodec (Operation' expectedFormat parsingStrictness)
--
-- `codec` for this instance would implicitly be a function (because we'll need to pass it an HasCodec dictionary for Pointer'),
-- and as a result, it's result will not be cached and recomputed each time it's called.
--
-- As making an object codec for `Operation'` involves things like creating a hashmap, whilst it's probably not a big deal
-- it's nice if we can do all this work once instead of repeating it every time someone calls `toJSON` or `parseJSON`.
--
-- See: https://stackoverflow.com/questions/77056264/caching-an-expensive-to-compute-result-in-a-class-instance
-- for details
instance Autodocodec.HasObjectCodec (Operation' RFC6901Format StrictParsing) where objectCodec = operationObjectCodec
instance Autodocodec.HasObjectCodec (Operation' RFC6901Format LenientParsing) where objectCodec = operationObjectCodec
instance Autodocodec.HasObjectCodec (Operation' ArrayFormat StrictParsing) where objectCodec = operationObjectCodec
instance Autodocodec.HasObjectCodec (Operation' ArrayFormat LenientParsing) where objectCodec = operationObjectCodec

operationObjectCodec :: forall expectedFormat parsingStrictness. 
  Autodocodec.HasCodec (Pointer' expectedFormat parsingStrictness) => Autodocodec.JSONObjectCodec (Operation' expectedFormat parsingStrictness)
operationObjectCodec = Autodocodec.discriminatedUnionCodec "op" enc dec where
  enc :: Operation' expectedFormat parsingStrictness -> (Autodocodec.Discriminator, Autodocodec.ObjectCodec (Operation' expectedFormat parsingStrictness) ())
  enc (Operation' o) = case o of 
    Add { changePointer, changeValue } -> (addDiscriminator, Autodocodec.mapToEncoder (changePointer, changeValue) addCodec)
    Cpy { changePointer, fromPointer } -> (copyDiscriminator, Autodocodec.mapToEncoder (changePointer, fromPointer) copyCodec)
    Mov { changePointer, fromPointer } -> (moveDiscriminator, Autodocodec.mapToEncoder (changePointer, fromPointer) moveCodec)
    Rem { changePointer } -> (removeDiscriminator, Autodocodec.mapToEncoder changePointer removeCodec)
    Rep { changePointer, changeValue } -> (replaceDiscriminator, Autodocodec.mapToEncoder (changePointer, changeValue) replaceCodec)
    Tst { changePointer, changeValue } -> (testDiscriminator, Autodocodec.mapToEncoder (changePointer, changeValue) testCodec)
  dec :: HashMap Autodocodec.Discriminator (Text, Autodocodec.ObjectCodec Void  (Operation' expectedFormat parsingStrictness))
  dec = HashMap.fromList
    [ (addDiscriminator, (addObjName, Autodocodec.mapToDecoder (Operation' . uncurry Add) addCodec))
    , (copyDiscriminator, (copyObjName, Autodocodec.mapToDecoder (Operation' . uncurry Cpy) copyCodec))
    , (moveDiscriminator, (moveObjName, Autodocodec.mapToDecoder (Operation' . uncurry Mov) moveCodec))
    , (removeDiscriminator, (removeObjName, Autodocodec.mapToDecoder (Operation' . Rem) removeCodec))
    , (replaceDiscriminator, (replaceObjName, Autodocodec.mapToDecoder (Operation' . uncurry Rep) replaceCodec))
    , (testDiscriminator, (testObjName, Autodocodec.mapToDecoder (Operation' . uncurry Tst) testCodec))
    ]
  addDiscriminator :: Text
  addDiscriminator = "add"
  addObjName :: Text
  addObjName = "JsonPatchAdd"
  addCodec :: Autodocodec.JSONObjectCodec (Pointer, Value)
  addCodec = coerce addCodec' where 
    addCodec' :: Autodocodec.JSONObjectCodec (Pointer' expectedFormat parsingStrictness, Value)
    addCodec' = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      changeValueVal <- Autodocodec.requiredField' "value" .= snd
      pure (changePointerVal, changeValueVal)
  copyDiscriminator :: Text
  copyDiscriminator = "copy"
  copyObjName :: Text
  copyObjName = "JsonPatchCopy"
  copyCodec :: Autodocodec.JSONObjectCodec (Pointer, Pointer)
  copyCodec = coerce copyCodec' where
    copyCodec' :: Autodocodec.JSONObjectCodec (Pointer' expectedFormat parsingStrictness, Pointer' expectedFormat parsingStrictness)
    copyCodec' = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      fromPointerVal <- Autodocodec.requiredField' "from" .= snd
      pure (changePointerVal, fromPointerVal)
  moveDiscriminator :: Text
  moveDiscriminator = "move"
  moveObjName :: Text
  moveObjName = "JsonPatchMove"
  moveCodec :: Autodocodec.JSONObjectCodec (Pointer, Pointer)
  moveCodec = coerce moveCodec' where
    moveCodec' :: Autodocodec.JSONObjectCodec (Pointer' expectedFormat parsingStrictness, Pointer' expectedFormat parsingStrictness)
    moveCodec' = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      fromPointerVal <- Autodocodec.requiredField' "from" .= snd
      pure (changePointerVal, fromPointerVal)
  removeDiscriminator :: Text
  removeDiscriminator = "remove"
  removeObjName :: Text
  removeObjName = "JsonPatchRemove"
  removeCodec :: Autodocodec.JSONObjectCodec Pointer
  removeCodec = coerce removeCodec' where
    removeCodec' :: Autodocodec.JSONObjectCodec (Pointer' expectedFormat parsingStrictness)
    removeCodec' = Autodocodec.requiredField' "path" .= id
  replaceDiscriminator :: Text
  replaceDiscriminator = "replace"
  replaceObjName :: Text
  replaceObjName = "JsonPatchReplace"
  replaceCodec :: Autodocodec.JSONObjectCodec (Pointer, Value)
  replaceCodec = coerce replaceCodec' where
    replaceCodec' :: Autodocodec.JSONObjectCodec (Pointer' expectedFormat parsingStrictness, Value)
    replaceCodec' = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      changeValueVal <- Autodocodec.requiredField' "value" .= snd
      pure (changePointerVal, changeValueVal)
  testDiscriminator :: Text
  testDiscriminator = "test"
  testObjName :: Text
  testObjName = "JsonPatchTest"
  testCodec :: Autodocodec.JSONObjectCodec (Pointer, Value)
  testCodec = coerce testCodec' where
    testCodec' :: Autodocodec.JSONObjectCodec (Pointer' expectedFormat parsingStrictness, Value)
    testCodec' = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      changeValueVal <- Autodocodec.requiredField' "value" .= snd
      pure (changePointerVal, changeValueVal)

-- | Modify the 'Pointer's in an 'Operation'.
--
-- If the operation contains multiple pointers (i.e. a 'Mov' or 'Cpy')
-- then both will be modified.
modifyPointer :: (Pointer -> Pointer) -> Operation -> Operation
modifyPointer f op =
  case op of
    Add{..} -> op{ changePointer = f changePointer }
    Cpy{..} -> op{ changePointer = f changePointer, fromPointer = f fromPointer }
    Mov{..} -> op{ changePointer = f changePointer, fromPointer = f fromPointer }
    Rem{..} -> op{ changePointer = f changePointer }
    Rep{..} -> op{ changePointer = f changePointer }
    Tst{..} -> op{ changePointer = f changePointer }

isAdd :: Operation -> Bool
isAdd Add{} = True
isAdd _ = False

isCpy :: Operation -> Bool
isCpy Cpy{} = True
isCpy _ = False

isMov :: Operation -> Bool
isMov Mov{} = True
isMov _ = False

isRem :: Operation -> Bool
isRem Rem{} = True
isRem _ = False

isRep :: Operation -> Bool
isRep Rep{} = True
isRep _ = False

isTst :: Operation -> Bool
isTst Tst{} = True
isTst _ = False
