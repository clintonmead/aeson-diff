module Data.Aeson.Pointer.ArrayOffset (
  ArrayOffset(ArrayOffset, PosOffset, NegOffset)
, toAbsArrayOffset
, (!?-)
, (//-)
) where
  
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bifunctor as Bifunctor

-- * Array offsets

-- | An offset into an array. 
--
-- Positive values are indices from the start of the array.
--
-- Negative values are indices from the end of the array.
--
-- Exactly what this means depends on context though.
--
-- When inserting into an array, we treat the array as if it is one larger than it is.
-- For example, for an array of length @5@, using @5@ as the index will add an element to the end of the array. 
-- However @5@ is not a valid index when replacing an element of a length @5@ array.
-- If one wants to replace the last element of a length @5@ array, one should use @4@.
-- Consequently, when inserting into an array, @-1@ will add an element to the end of the array.
-- whereas when replacing an element of the array, @-1@ will replace the last element of an array.
--
-- The consequence of this is when we say have a @5@ element array, and we are adding an element,
-- then index @5@ is the same as index @-1@.
-- 
-- However, when we have the same @5@ element array, and we are replacing an element,
-- then index @4@ is the same as index @-1@. 
newtype ArrayOffset = ArrayOffset Int
  -- deriving Show via newtype does make the testing strings a bit less ugly which is nice
  -- We haven't got 'Read' so it doesn't really matter what we use for 'Show' it's just for debugging.
  deriving newtype (Show, Eq) 

data SplitArrayOffset = PosOffset' Int | NegOffset' Int

getSign :: ArrayOffset -> SplitArrayOffset
getSign (ArrayOffset n) = case n >= 0 of
  True -> PosOffset' n
  False -> NegOffset' n

{-# COMPLETE PosOffset, NegOffset #-}
pattern PosOffset :: Int -> ArrayOffset
pattern PosOffset n <- (getSign -> PosOffset' n)

pattern NegOffset :: Int -> ArrayOffset
pattern NegOffset n <- (getSign -> NegOffset' n)

toAbsArrayOffset :: Vector a -> ArrayOffset -> Int
toAbsArrayOffset v = \case
  PosOffset i -> i
  NegOffset i -> V.length v + i

(!?-) :: Vector a -> ArrayOffset -> Maybe a
(!?-) v i = v V.!? toAbsArrayOffset v i

(//-) :: Vector a -> [(ArrayOffset, a)] -> Vector a
(//-) v l = v V.// (Bifunctor.first (toAbsArrayOffset v) <$> l)
