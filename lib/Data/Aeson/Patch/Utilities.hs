module Data.Aeson.Patch.Utilities 
(
applyAdd, applyRem, applyRep, applyMov, applyCpy, applyTst) where
  
import Data.Aeson.Pointer (Pointer (Pointer), Key (AKey, OKey), pointerFailure, get, formatPointer)
import Data.Aeson (Value (Array, Object, Null), Result (Error))
import Data.Aeson.Pointer.ArrayOffset.Utilities (vInsert, vModify)
import qualified Data.Aeson.KeyMap as HM
import Data.Aeson.Patch.Generic.Utilities (hmModify)
import Control.Monad (unless)
import qualified Data.Text as T

-- | Apply an 'Add' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.1
--
-- - An empty 'Path' replaces the document.
-- - A single 'OKey' inserts or replaces the corresponding member in an object.
-- - A single 'AKey' inserts at the corresponding location.
-- - Longer 'Paths' traverse if they can and fail otherwise.
applyAdd :: Pointer -> Value -> Value -> Result Value
applyAdd pointer = go pointer
  where
    go (Pointer []) val _ =
        return val
    go (Pointer [AKey i]) v' (Array v) =
        return (Array $ vInsert i v' v)
    go (Pointer (AKey i : path)) v' (Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "insert" "array" i pointer
            fn (Just d) = Just <$> go (Pointer path) v' d
        in Array <$> vModify i fn v
    go (Pointer [OKey n]) v' (Object m) =
        return . Object $ HM.insert n v' m
    go (Pointer (OKey n : path)) v' (Object o) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "insert" "object" n pointer
            fn (Just d) = Just <$> go (Pointer path) v' d
        in Object <$> hmModify n fn o
    go path _ v = pointerFailure path v

-- | Apply a 'Rem' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.2
--
-- - The target location MUST exist.
applyRem :: Pointer -> Value -> Result Value
applyRem from@(Pointer path) = go path
  where
    go [] _ = return Null
    go [AKey i] (Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "delete" "array" i from
            fn (Just _) = return Nothing
        in Array <$> vModify i fn v
    go (AKey i : path) (Array v) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "traverse" "array" i from
            fn (Just o) = Just <$> go path o
        in Array <$> vModify i fn v
    go [OKey n] (Object m) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "delete" "object" n from
            fn (Just _) = return Nothing
        in Object <$> hmModify n fn m
    go (OKey n : path) (Object m) =
        let fn :: Maybe Value -> Result (Maybe Value)
            fn Nothing  = cannot "traverse" "object" n from
            fn (Just o) = Just <$> go path o
        in Object <$> hmModify n fn m
    -- Type mismatch: clearly the thing we're deleting isn't here.
    go _path value = pointerFailure from value

-- | Apply a 'Rep' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.3
--
-- - Functionally identical to a 'Rem' followed by an 'Add'.
applyRep :: Pointer -> Value -> Value -> Result Value
applyRep from v doc = applyRem from doc >>= applyAdd from v

-- | Apply a 'Mov' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.4
applyMov :: Pointer -> Pointer -> Value -> Result Value
applyMov path from doc = do
  v <- get from doc
  applyRem from doc >>= applyAdd path v

-- | Apply a 'Cpy' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.5
--
-- - The location must exist.
-- - Identical to an add with the appropriate value.
applyCpy :: Pointer -> Pointer -> Value -> Result Value
applyCpy path from doc = do
  v <- get from doc
  applyAdd path v doc

-- | Apply a 'Tst' operation to a document.
--
-- http://tools.ietf.org/html/rfc6902#section-4.6
--
-- - The location must exist.
-- - The value must be equal to the supplied value.
applyTst :: Pointer -> Value -> Value -> Result Value
applyTst path v doc = do
    v' <- get path doc
    unless (v == v') (Error . T.unpack $ "Element at \"" <> formatPointer path <> "\" fails test.")
    return doc

-- | Report an error about being able to use a pointer key.
cannot
    :: (Show ix)
    => String -- ^ Use to be made "delete", "traverse", etc.
    -> String -- ^ Type "array" "object"
    -> ix
    -> Pointer
    -> Result a
cannot op ty ix p =
    Error ("Cannot " <> op <> " missing " <> ty <> " member at index "
          <> show ix <> " in pointer \"" <> T.unpack (formatPointer p) <> "\".")
