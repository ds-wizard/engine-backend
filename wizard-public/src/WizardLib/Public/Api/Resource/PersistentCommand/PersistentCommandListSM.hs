module WizardLib.Public.Api.Resource.PersistentCommand.PersistentCommandListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import WizardLib.Public.Api.Resource.PersistentCommand.PersistentCommandListJM ()
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionSM ()
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()
import WizardLib.Public.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

instance ToSchema PersistentCommandList where
  declareNamedSchema = toSwagger command1List
