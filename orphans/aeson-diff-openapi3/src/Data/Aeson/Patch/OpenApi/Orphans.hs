module Data.Aeson.Patch.OpenApi.Orphans () where

import Autodocodec.OpenAPI.DerivingVia (AutodocodecOpenApi)
import Data.Aeson.Patch (Patch)
import Data.OpenApi qualified as OpenApi

deriving via AutodocodecOpenApi Patch instance OpenApi.ToSchema Patch
