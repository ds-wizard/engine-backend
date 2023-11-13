module Wizard.Api.Resource.PersistentCommand.PersistentCommandSM where

import Data.Swagger
import qualified Data.UUID as U

import Shared.Common.Util.Swagger
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Wizard.Api.Resource.Tenant.TenantSM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.PersistentCommand.PersistentCommandMapper
import qualified Wizard.Service.Tenant.TenantMapper as TNT_Mapper

instance ToSchema PersistentCommandDTO where
  declareNamedSchema = toSwagger (toDTO command1 (Just userAlbert) (TNT_Mapper.toDTO defaultTenant Nothing Nothing))

instance ToSchema (PersistentCommand U.UUID) where
  declareNamedSchema = toSwagger command1
