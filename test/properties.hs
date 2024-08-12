{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Control.Monad              (unless)
import           Data.Aeson                 (Result(Success), Value(Array, Object), encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson.KeyMap          as HM
import qualified Data.Vector                as V
import           System.Exit                (exitFailure)
import           Test.QuickCheck            (Arbitrary, Gen, arbitrary, oneof, quickCheckAll, resize, sized)

import Data.Aeson.Diff                      (Config(Config), diff, diff', patch)
import Data.Aeson.Patch                     (isRem, isTst, patchOperations)
import qualified Data.Foldable as Foldable

showIt :: Value -> String
showIt = BL.unpack . encode

newtype Wellformed a = Wellformed { wellformed :: a }

newtype AnObject a = AnObject { anObject :: a }

newtype AnArray a = AnArray { anArray :: a }

instance Show (Wellformed Value) where
    show = showIt . wellformed

instance Show (AnObject Value) where
    show = showIt . anObject

instance Show (AnArray Value) where
    show = showIt . anArray

-- | QuickCheck doesn't have scale in LTS-2 so copy it in.
scaleSize :: (Int -> Int) -> Gen a -> Gen a
scaleSize f g = sized (\s -> resize (f s) g)

instance Arbitrary (Wellformed Value) where
    arbitrary = Wellformed <$> oneof
                [ Array . V.fromList <$> scaleSize (`div` 2) arbitrary
                , Object . HM.fromList <$> scaleSize (`div` 2) arbitrary
                ]

instance Arbitrary (AnObject Value) where
    arbitrary = AnObject . Object . HM.fromList <$> scaleSize (`div` 2) arbitrary

instance Arbitrary (AnArray Value) where
    arbitrary = AnArray . Array . V.fromList <$> scaleSize (`div` 2) arbitrary


-- | Extracting and applying a patch is an identity.
diffApply
    :: Value
    -> Value
    -> Bool
diffApply f t =
    let p = diff f t
    in (Success t == patch p f) ||
       error ("BAD PATCH\n" <> BL.unpack (encode p) <> "\n"
                            <> result "<failure>" (BL.unpack . encode <$> patch p f))

result :: a -> Result a -> a
result _ (Success a) = a
result a _             = a

-- | Patch extracted from identical documents should be mempty.
prop_diff_id
    :: Wellformed Value
    -> Bool
prop_diff_id (Wellformed v) =
    diff v v == mempty

-- | Extract and apply a patch (between wellformed JSON documents).
prop_diff_documents
    :: Wellformed Value
    -> Wellformed Value
    -> Bool
prop_diff_documents (Wellformed f) (Wellformed t) =
    diffApply f t

-- | Extract and apply a patch (specialised to JSON arrays).
prop_diff_arrays
    :: AnArray Value
    -> AnArray Value
    -> Bool
prop_diff_arrays (AnArray v1) (AnArray v2) =
    diffApply v1 v2

-- | Extract and apply a patch (specialised to JSON objects).
prop_diff_objects
    :: AnObject Value
    -> AnObject Value
    -> Bool
prop_diff_objects (AnObject m1) (AnObject m2) =
    diffApply m1 m2

-- | Check that 'Rem' always preceded by a 'Tst'.
prop_tst_before_rem
  :: Wellformed Value
  -> Wellformed Value
  -> Bool
prop_tst_before_rem (Wellformed f) (Wellformed t) =
  let ops = zip [1..] (Foldable.toList (patchOperations $ diff' (Config True) f t))
      rs = map fst . filter (isRem . snd) $ ops
      ts = map fst . filter (isTst . snd) $ ops
      minusOneInTs :: Integer -> Bool
      minusOneInTs r = (r - 1) `elem` ts
  in (length rs <= length ts) && all minusOneInTs rs

--
-- Use Template Haskell to automatically run all of the properties above.
--

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    result <- runTests
    unless result exitFailure
