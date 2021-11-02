module Wizard.Api.Resource.Admin.AdminSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Admin.AdminJM ()
import Wizard.Database.Migration.Development.Admin.Data.Admins
import Wizard.Model.Admin.Admin

instance ToSchema AdminSection where
  declareNamedSchema = simpleToSchema' "_adminSection" section

instance ToSchema AdminOperation where
  declareNamedSchema = simpleToSchema' "_adminOperation" operation

instance ToSchema AdminOperationParameter where
  declareNamedSchema = simpleToSchema' "_adminOperationParameter" operationParam1

instance ToSchema AdminOperationParameterType
