module Codec (encode, decode, AesonFormat (..)) where

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
decode _ (Just fn) | ".yaml" `isSuffixOf` (toLower <$> fn) ||
                     ".yml"  `isSuffixOf` (toLower <$> fn) = decodeYamlFirst
decode _ (Just fn) | ".json" `isSuffixOf` (toLower <$> fn) = decodeJsonFirst
decode _ _ = decodeJsonFirst


decodeYamlFirst :: Aeson.FromJSON a => BL.ByteString -> Maybe a
decodeYamlFirst s = Yaml.decodeThrow (BL.toStrict s) <|> Aeson.decode s

decodeJsonFirst :: Aeson.FromJSON a => BL.ByteString -> Maybe a
decodeJsonFirst s = Aeson.decode s <|> Yaml.decodeThrow (BL.toStrict s)

encode :: Aeson.ToJSON a => Maybe AesonFormat -> Maybe FilePath -> a -> BL.ByteString
encode (Just AesonYAML) _ = BL.fromStrict . Yaml.encode
encode (Just AesonJSON) _ = Aeson.encode
encode _ (Just fn) | ".yaml" `isSuffixOf` (toLower <$> fn) ||
                     ".yml"  `isSuffixOf` (toLower <$> fn) = BL.fromStrict . Yaml.encode
encode _ (Just fn) | ".json" `isSuffixOf` (toLower <$> fn) = Aeson.encode
encode _ _ = Aeson.encode
