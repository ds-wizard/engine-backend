module Shared.Api.Resource.Package.PackagePhaseSM where

import Data.Swagger

import Shared.Api.Resource.Package.PackageJM ()
import Shared.Model.Package.Package

instance ToSchema PackagePhase

instance ToParamSchema PackagePhase
