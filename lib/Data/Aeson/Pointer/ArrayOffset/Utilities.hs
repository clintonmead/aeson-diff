module Data.Aeson.Pointer.ArrayOffset.Utilities 
( vDelete
, vInsert
, vModify
, toAbsArrayOffset
, (!?-)
, (//-)
) where
  
import Data.Aeson.Pointer.ArrayOffset (ArrayOffset (PosOffset, NegOffset))
import Data.Vector (Vector)
import Data.Aeson (Result (Success, Error))
import qualified Data.Vector as V
import qualified Data.Bifunctor as Bifunctor

-- * Utilities

-- $ These are some utility functions.
-- Mostly they just fill gaps in the APIs of the "Data.Vector"
-- and "Data.Aeson.KeyMap" modules.

-- | Delete an element in a vector.
vDelete :: ArrayOffset -> Vector a -> Vector a
vDelete i' v =
    let
      i = toAbsArrayOffset v i' 
      l = V.length v
    in 
      V.slice 0 i v <> V.slice (i + 1) (l - i - 1) v

-- | Insert an element into a vector.
vInsert :: ArrayOffset -> a -> Vector a -> Vector a
vInsert i' a v =
    let
      i = case i' of
        PosOffset n -> n
        NegOffset n -> V.length v + 1 + n
    in
      V.slice 0 i v
      <> V.singleton a
      <> V.slice i (V.length v - i) v

-- | Modify the element at an index in a 'Vector'.
--
-- The function is passed the value at index @i@, or 'Nothing' if there is no
-- such element. The function should return 'Nothing' if it wants to have no
-- value corresponding to the index, or 'Just' if it wants a value.
--
-- Depending on the vector and the function, we will either:
--
-- - leave the vector unchanged;
-- - delete an existing element;
-- - insert a new element; or
-- - replace an existing element.
vModify
    :: ArrayOffset
    -> (Maybe a -> Result (Maybe a))
    -> Vector a
    -> Result (Vector a)
vModify i f v =
    let a = v !?- i
        a' = f a
    in case (a, a') of
        (Nothing, Success Nothing ) -> return v
        (Just _ , Success Nothing ) -> return (vDelete i v)
        (Nothing, Success (Just n)) -> return (vInsert i n v)
        (Just _ , Success (Just n)) -> return (v //- [(i, n)])
        (_      , Error   e       ) -> Error e

toAbsArrayOffset :: Vector a -> ArrayOffset -> Int
toAbsArrayOffset v = \case
  PosOffset i -> i
  NegOffset i -> V.length v + i

(!?-) :: Vector a -> ArrayOffset -> Maybe a
(!?-) v i = v V.!? toAbsArrayOffset v i

(//-) :: Vector a -> [(ArrayOffset, a)] -> Vector a
(//-) v l = v V.// (Bifunctor.first (toAbsArrayOffset v) <$> l)
