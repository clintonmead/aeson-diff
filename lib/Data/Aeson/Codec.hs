module Data.Aeson.Codec (encode, decode, AesonFormat (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Control.Applicative ((<|>))
import           Data.Char           (toLower)
import           Data.List           (isSuffixOf)

data AesonFormat = AesonJSON | AesonYAML deriving (Show, Eq)

decode :: Aeson.FromJSON a => Maybe AesonFormat -> Maybe FilePath -> BL.ByteString -> Maybe a
decode (Just AesonYAML) _ = decodeYamlFirst
decode (Just AesonJSON) _ = decodeJsonFirst
decode _ (Just fn) | isSuffixOf ".yaml" (toLower <$> fn) ||
                     isSuffixOf ".yml"  (toLower <$> fn) = decodeYamlFirst
decode _ (Just fn) | isSuffixOf ".json" (toLower <$> fn) = decodeJsonFirst
decode _ _ = decodeJsonFirst


decodeYamlFirst :: Aeson.FromJSON a => BL.ByteString -> Maybe a
decodeYamlFirst s = Yaml.decode (BL.toStrict s) <|> Aeson.decode s

decodeJsonFirst :: Aeson.FromJSON a => BL.ByteString -> Maybe a
decodeJsonFirst s = Aeson.decode s <|> Yaml.decode (BL.toStrict s)

encode :: Aeson.ToJSON a => Maybe AesonFormat -> Maybe FilePath -> a -> BL.ByteString
encode (Just AesonYAML) _ = BL.fromStrict . Yaml.encode
encode (Just AesonJSON) _ = Aeson.encode
encode _ (Just fn) | isSuffixOf ".yaml" (toLower <$> fn) ||
                     isSuffixOf ".yml"  (toLower <$> fn) = BL.fromStrict . Yaml.encode
encode _ (Just fn) | isSuffixOf ".json" (toLower <$> fn) = Aeson.encode
encode _ _ = Aeson.encode
