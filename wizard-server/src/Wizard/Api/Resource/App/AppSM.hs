module Wizard.Api.Resource.App.AppSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Service.App.AppMapper

instance ToSchema App where
  declareNamedSchema = toSwagger defaultApp

instance ToSchema AppDTO where
  declareNamedSchema = toSwagger (toDTO defaultApp Nothing Nothing)
