module Wizard.Api.Resource.App.AppChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppChangeJM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Service.App.AppMapper

instance ToSchema AppChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO defaultApp)
