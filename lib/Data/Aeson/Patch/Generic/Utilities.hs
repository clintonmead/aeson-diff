module Data.Aeson.Patch.Generic.Utilities 
( hmModify
) where
  
import qualified Data.Aeson.Key as AesonKey
import Data.Aeson (Result (Error, Success))
import qualified Data.Aeson.KeyMap as HM

-- | Modify the value associated with a key in a 'KeyMap'.
--
-- The function is passed the value defined for @k@, or 'Nothing'. If the
-- function returns 'Nothing', the key and value are deleted from the map;
-- otherwise the value replaces the existing value in the returned map.
hmModify
    :: AesonKey.Key
    -> (Maybe v -> Result (Maybe v))
    -> HM.KeyMap v
    -> Result (HM.KeyMap v)
hmModify k f m = case f (HM.lookup k m) of
    Error e          -> Error e
    Success Nothing  -> return $ HM.delete k m
    Success (Just v) -> return $ HM.insert k v m

