module Wizard.Api.Resource.Dev.DevSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Dev.DevJM ()
import Wizard.Database.Migration.Development.Dev.Data.Devs
import Wizard.Model.Dev.Dev

instance ToSchema DevSection where
  declareNamedSchema = toSwagger section

instance ToSchema DevOperation where
  declareNamedSchema = toSwagger operation

instance ToSchema DevOperationParameter where
  declareNamedSchema = toSwagger operationParam1

instance ToSchema DevOperationParameterType
