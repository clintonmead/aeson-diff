-- | Description: Extract and apply patches on JSON documents.
--
-- This module implements data types and operations to represent the
-- differences between JSON documents (i.e. a patch), to compare JSON documents
-- and extract such a patch, and to apply such a patch to a JSON document.
module Data.Aeson.Diff (
    -- * Patches
    Patch(..),
    Pointer,
    Key(..),
    Operation(..),
    Config(..),
    -- * Functions
    diff,
    diff',
    patch, -- This is re-exported from Data.Aeson.Patch for backward compatability
    applyOperation -- This is re-exported from Data.Aeson.Patch for backward compatability
) where

import           Data.Aeson                 (Array, Object, ToJSON(toJSON), Value(Array, Object, String, Null, Bool, Number))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as HM
import           Data.List                  (groupBy)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                (Sum(Sum))
import qualified Data.Vector                as V
import           Data.Vector.Distance       (Params(Params, equivalent, positionOffset, substitute, insert, delete, cost), leastChanges)
import Data.Aeson.Patch                     (Operation(Add, Cpy, Mov, Rem, Rep, Tst), Patch(Patch), changePointer, changeValue, modifyPointer, patch, applyOperation)
import Data.Aeson.Pointer                   (Key(AKey, OKey), Pointer(Pointer), pointerPath)
import Data.Aeson.Pointer.ArrayOffset (ArrayOffset (ArrayOffset))
import qualified Data.DList as DList

-- * Configuration

-- | Configuration for the diff algorithm.
newtype Config = Config
  { configTstBeforeRem :: Bool
  }

defaultConfig :: Config
defaultConfig = Config False

-- * Costs

-- | Calculate the cost of an operation.
operationCost :: Operation -> Int
operationCost op =
    case op of
      Add{} -> valueSize (changeValue op)
      Rem{} -> 1
      Rep{} -> valueSize (changeValue op)
      Mov{} -> 1
      Cpy{} -> 1
      Tst{} -> valueSize (changeValue op)

-- | Estimate the size of a JSON 'Value'.
valueSize :: Value -> Int
valueSize val = case val of
    Object o -> sum . fmap valueSize . HM.elems $ o
    Array  a -> V.sum $ V.map valueSize a
    _        -> 1

-- * Atomic patches

-- | Construct a patch with a single 'Add' operation.
ins :: Config -> Pointer -> Value -> [Operation]
ins _cfg p v = [Add p v]

-- | Construct a patch with a single 'Rem' operation.
del :: Config -> Pointer -> Value -> [Operation]
del Config{configTstBeforeRem} p v =
  if configTstBeforeRem
  then [Tst p v, Rem p]
  else [Rem p]

-- | Construct a patch which changes 'Rep' operation.
rep :: Config -> Pointer -> Value -> [Operation]
rep _cfg p v = [Rep p v]

-- * Diff

-- | Compare two JSON documents and generate a patch describing the differences.
--
-- Uses the 'defaultConfig'.
diff
  :: Value
  -> Value
  -> Patch
diff = diff' defaultConfig

-- | Compare two JSON documents and generate a patch describing the differences.
diff'
    :: Config
    -> Value
    -> Value
    -> Patch
diff' cfg v v' = Patch . DList.fromList $ worker mempty v v'
  where
    check :: Monoid m => Bool -> m -> m
    check b v = if b then mempty else v

    worker :: Pointer -> Value -> Value -> [Operation]
    worker p v1 v2 = case (v1, v2) of
        -- For atomic values of the same type, emit changes iff they differ.
        (Null,      Null)      -> mempty
        (Bool b1,   Bool b2)   -> check (b1 == b2) $ rep cfg p v2
        (Number n1, Number n2) -> check (n1 == n2) $ rep cfg p v2
        (String s1, String s2) -> check (s1 == s2) $ rep cfg p v2

        -- For structured values of the same type, walk them.
        (Array a1,  Array a2)  -> check (a1 == a2) $ workArray  p a1 a2
        (Object o1, Object o2) -> check (o1 == o2) $ workObject p o1 o2

        -- For values of different types, replace v1 with v2.
        _                      -> rep cfg p v2

    -- Walk the keys in two objects, producing a 'Patch'.
    workObject :: Pointer -> Object -> Object -> [Operation]
    workObject path o1 o2 =
        let k1 = HM.keys o1
            k2 = HM.keys o2
            -- Deletions
            del_keys :: [AesonKey.Key]
            del_keys = filter (not . (`elem` k2)) k1
            deletions :: [Operation]
            deletions = concatMap
                (\k -> del cfg (Pointer [OKey k]) (fromJust $ HM.lookup k o1))
                del_keys
            -- Insertions
            ins_keys = filter (not . (`elem` k1)) k2
            insertions :: [Operation]
            insertions = concatMap
                (\k -> ins cfg (Pointer [OKey k]) (fromJust $ HM.lookup k o2))
                ins_keys
            -- Changes
            chg_keys = filter (`elem` k2) k1
            changes :: [Operation]
            changes = concatMap
                (\k -> worker (Pointer [OKey k])
                    (fromJust $ HM.lookup k o1)
                    (fromJust $ HM.lookup k o2))
                chg_keys
        in modifyPointer (path <>) <$> (deletions <> insertions <> changes)

    -- Use an adaption of the Wagner-Fischer algorithm to find the shortest
    -- sequence of changes between two JSON arrays.
    workArray :: Pointer -> Array -> Array -> [Operation]
    workArray path ss tt = fmap (modifyPointer (path <>)) . snd . fmap concat $ leastChanges params ss tt
      where
        params :: Params Value [Operation] (Sum Int)
        params = Params{equivalent, delete, insert, substitute, cost, positionOffset}
        equivalent :: Value -> Value -> Bool
        equivalent = (==)
        delete i = del cfg (Pointer [AKey (ArrayOffset i)])
        insert i = ins cfg (Pointer [AKey (ArrayOffset i)])
        substitute i = worker (Pointer [AKey (ArrayOffset i)])
        cost :: [Operation] -> Sum Int
        cost = Sum . sum . fmap operationCost
        -- Position is advanced by grouping operations with same "head" index:
        -- + groups of many operations advance one
        -- + singletons with |pointer|>1 advance one
        -- + other singletons advance according to 'pos'
        positionOffset = sum . fmap adv . groupBy related
        related :: Operation -> Operation -> Bool
        related o1 o2 =
            let p1 = pointerPath (changePointer o1)
                p2 = pointerPath (changePointer o2)
            in case (p1, p2) of
                 ([_], [_]) -> False
                 (i1:_, i2:_) | i1 == i2  -> True
                              | otherwise -> False
        -- A group of operations has a peculiar (i.e. given by 'pos') advance
        -- when it's a single op and |changePointer| = 1; otherwise it's a
        -- bunch of changes inside the head key.
        adv :: [Operation] -> Int
        adv [op]
            | (length . pointerPath . changePointer $ op) == 1 = pos op
        adv _    = 1
        pos :: Operation -> Int
        pos Rem{changePointer=Pointer path}
            | length path == 1 = 0
            | otherwise        = 0
        pos Add{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Rep{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Cpy{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Mov{changePointer=Pointer path}
            | length path == 1 = 1
            | otherwise        = 0
        pos Tst{changePointer=Pointer _path} = 0

-- | Compare two documents using their `ToJSON` instance.
--
-- Uses the 'defaultConfig'.
toJSONDiff :: ToJSON a => a -> a -> Patch
toJSONDiff a1 a2 = diff (toJSON a1) (toJSON a2)

-- | Compare two documents using their `ToJSON` instance.
toJSONDiff' :: ToJSON a => Config -> a -> a -> Patch
toJSONDiff' cfg a1 a2 = diff' cfg (toJSON a1) (toJSON a2)
