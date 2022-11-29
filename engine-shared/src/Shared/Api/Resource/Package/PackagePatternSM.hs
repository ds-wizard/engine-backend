module Shared.Api.Resource.Package.PackagePatternSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackagePatternJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Package.PackagePattern
import Shared.Util.Swagger

instance ToSchema PackagePattern where
  declareNamedSchema = toSwagger packagePatternAll
