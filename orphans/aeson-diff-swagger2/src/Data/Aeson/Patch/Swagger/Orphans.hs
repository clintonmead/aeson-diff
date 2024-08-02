module Data.Aeson.Patch.Swagger.Orphans () where

import Autodocodec.Swagger.DerivingVia (AutodocodecSwagger)
import Data.Swagger qualified as Swagger
import Data.Aeson.Patch (Patch')
import Data.Typeable (Typeable)
import Data.Aeson.Pointer (Pointer', ExpectedFormat (RFC6901Format, ArrayFormat), ParsingStrictness (StrictParsing, LenientParsing))

-- Have separate instances so we ensure we only calculate the ToSchema function once
-- (See: https://stackoverflow.com/questions/77056264/caching-an-expensive-to-compute-result-in-a-class-instance for details)
deriving via AutodocodecSwagger (Patch' RFC6901Format StrictParsing) instance Swagger.ToSchema (Patch' RFC6901Format StrictParsing)
deriving via AutodocodecSwagger (Patch' ArrayFormat StrictParsing) instance Swagger.ToSchema (Patch' ArrayFormat StrictParsing)
deriving via AutodocodecSwagger (Patch' RFC6901Format LenientParsing) instance Swagger.ToSchema (Patch' RFC6901Format LenientParsing)
deriving via AutodocodecSwagger (Patch' ArrayFormat LenientParsing) instance Swagger.ToSchema (Patch' ArrayFormat LenientParsing)
