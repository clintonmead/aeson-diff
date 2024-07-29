module Data.Aeson.Patch.Swagger.Orphans where

import Autodocodec.Swagger.DerivingVia (AutodocodecSwagger)
import Data.Aeson.Patch (Patch)
import Data.Swagger qualified as Swagger

deriving via AutodocodecSwagger Patch instance Swagger.ToSchema Patch
