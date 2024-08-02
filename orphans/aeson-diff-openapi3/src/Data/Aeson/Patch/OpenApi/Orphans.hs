module Data.Aeson.Patch.OpenApi.Orphans () where

import Autodocodec.OpenAPI.DerivingVia (AutodocodecOpenApi)
import Data.OpenApi qualified as OpenApi
import Data.Aeson.Patch (Patch')
import Data.Typeable (Typeable)
import Data.Aeson.Pointer (Pointer', ExpectedFormat(RFC6901Format, ArrayFormat), ParsingStrictness(StrictParsing, LenientParsing))

-- Have separate instances so we ensure we only calculate the ToSchema function once
-- (See: https://stackoverflow.com/questions/77056264/caching-an-expensive-to-compute-result-in-a-class-instance for details)
deriving via AutodocodecOpenApi (Patch' RFC6901Format StrictParsing) instance OpenApi.ToSchema (Patch' RFC6901Format StrictParsing)
deriving via AutodocodecOpenApi (Patch' ArrayFormat StrictParsing) instance OpenApi.ToSchema (Patch' ArrayFormat StrictParsing)
deriving via AutodocodecOpenApi (Patch' RFC6901Format LenientParsing) instance OpenApi.ToSchema (Patch' RFC6901Format LenientParsing)
deriving via AutodocodecOpenApi (Patch' ArrayFormat LenientParsing) instance OpenApi.ToSchema (Patch' ArrayFormat LenientParsing)
