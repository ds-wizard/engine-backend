module Wizard.Api.Resource.Version.VersionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Api.Resource.Version.VersionJM ()
import Wizard.Database.Migration.Development.Version.Data.Versions

instance ToSchema VersionDTO where
  declareNamedSchema = simpleToSchema versionAmsterdam
