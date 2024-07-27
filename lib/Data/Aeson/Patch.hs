{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

-- | Description: Represent RFC 6902 patches.
module Data.Aeson.Patch (
  Patch(..),
  Operation(..),
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
) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Aeson.Types (Value)
import Data.Aeson.Pointer (Pointer)
import qualified Autodocodec
import Autodocodec ((.=), Autodocodec)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.HashMap.Strict as HashMap

-- * Patches

-- | Describes the changes between two JSON documents.
newtype Patch = Patch
    { patchOperations :: [Operation] }
  deriving stock Show
  deriving newtype (Eq, Semigroup, Monoid, Autodocodec.HasCodec)
  deriving (ToJSON, FromJSON) via Autodocodec Patch

-- | Modify the pointers in the 'Operation's of a 'Patch'.
--
-- See 'modifyPointer' for details.
modifyPointers :: (Pointer -> Pointer) -> Patch -> Patch
modifyPointers f (Patch ops) = Patch (map (modifyPointer f) ops)

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
  deriving (ToJSON, FromJSON) via Autodocodec Operation

instance Autodocodec.HasCodec Operation where
  codec :: Autodocodec.JSONCodec Operation
  codec = Autodocodec.object "JsonPatchOperation" Autodocodec.objectCodec

instance Autodocodec.HasObjectCodec Operation where
  objectCodec = Autodocodec.discriminatedUnionCodec "op" enc dec where
    enc :: Operation -> (Autodocodec.Discriminator, Autodocodec.ObjectCodec Operation ())
    enc = \case
      Add { changePointer, changeValue } -> (addDiscriminator, Autodocodec.mapToEncoder (changePointer, changeValue) addCodec)
      Cpy { changePointer, fromPointer } -> (copyDiscriminator, Autodocodec.mapToEncoder (changePointer, fromPointer) copyCodec)
      Mov { changePointer, fromPointer } -> (moveDiscriminator, Autodocodec.mapToEncoder (changePointer, fromPointer) moveCodec)
      Rem { changePointer } -> (removeDiscriminator, Autodocodec.mapToEncoder changePointer removeCodec)
      Rep { changePointer, changeValue } -> (replaceDiscriminator, Autodocodec.mapToEncoder (changePointer, changeValue) replaceCodec)
      Tst { changePointer, changeValue } -> (testDiscriminator, Autodocodec.mapToEncoder (changePointer, changeValue) testCodec)
    dec :: HashMap Autodocodec.Discriminator (Text, Autodocodec.ObjectCodec Void Operation)
    dec = HashMap.fromList
      [ (addDiscriminator, (addObjName, Autodocodec.mapToDecoder (uncurry Add) addCodec))
      , (copyDiscriminator, (copyObjName, Autodocodec.mapToDecoder (uncurry Cpy) copyCodec))
      , (moveDiscriminator, (moveObjName, Autodocodec.mapToDecoder (uncurry Mov) moveCodec))
      , (removeDiscriminator, (removeObjName, Autodocodec.mapToDecoder Rem removeCodec))
      , (replaceDiscriminator, (replaceObjName, Autodocodec.mapToDecoder (uncurry Rep) replaceCodec))
      , (testDiscriminator, (testObjName, Autodocodec.mapToDecoder (uncurry Tst) testCodec))
      ]
    addDiscriminator :: Text
    addDiscriminator = "add"
    addObjName :: Text
    addObjName = "JsonPatchAdd"
    addCodec :: Autodocodec.JSONObjectCodec (Pointer, Value)
    addCodec = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      changeValueVal <- Autodocodec.requiredField' "value" .= snd
      pure (changePointerVal, changeValueVal)
    copyDiscriminator :: Text
    copyDiscriminator = "copy"
    copyObjName :: Text
    copyObjName = "JsonPatchCopy"
    copyCodec :: Autodocodec.JSONObjectCodec (Pointer, Pointer)
    copyCodec = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      fromPointerVal <- Autodocodec.requiredField' "from" .= snd
      pure (changePointerVal, fromPointerVal)
    moveDiscriminator :: Text
    moveDiscriminator = "move"
    moveObjName :: Text
    moveObjName = "JsonPatchMove"
    moveCodec :: Autodocodec.JSONObjectCodec (Pointer, Pointer)
    moveCodec = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      fromPointerVal <- Autodocodec.requiredField' "from" .= snd
      pure (changePointerVal, fromPointerVal)
    removeDiscriminator :: Text
    removeDiscriminator = "remove"
    removeObjName :: Text
    removeObjName = "JsonPatchRemove"
    removeCodec :: Autodocodec.JSONObjectCodec Pointer
    removeCodec = Autodocodec.requiredField' "path" .= id
    replaceDiscriminator :: Text
    replaceDiscriminator = "replace"
    replaceObjName :: Text
    replaceObjName = "JsonPatchReplace"
    replaceCodec :: Autodocodec.JSONObjectCodec (Pointer, Value)
    replaceCodec = do
      changePointerVal <- Autodocodec.requiredField' "path" .= fst
      changeValueVal <- Autodocodec.requiredField' "value" .= snd
      pure (changePointerVal, changeValueVal)
    testDiscriminator :: Text
    testDiscriminator = "test"
    testObjName :: Text
    testObjName = "JsonPatchTest"
    testCodec :: Autodocodec.JSONObjectCodec (Pointer, Value)
    testCodec = do
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
