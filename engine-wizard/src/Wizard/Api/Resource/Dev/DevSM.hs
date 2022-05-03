module Wizard.Api.Resource.Dev.DevSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Dev.DevJM ()
import Wizard.Database.Migration.Development.Dev.Data.Devs
import Wizard.Model.Dev.Dev

instance ToSchema DevSection where
  declareNamedSchema = simpleToSchema' "_devSection" section

instance ToSchema DevOperation where
  declareNamedSchema = simpleToSchema' "_devOperation" operation

instance ToSchema DevOperationParameter where
  declareNamedSchema = simpleToSchema' "_devOperationParameter" operationParam1

instance ToSchema DevOperationParameterType
