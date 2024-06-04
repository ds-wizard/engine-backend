module Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.Tenant.TenantSM ()
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.PersistentCommand.PersistentCommandMapper
import qualified Wizard.Service.Tenant.TenantMapper as TNT_Mapper
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema PersistentCommandDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO command1 (Just userAlbert) (TNT_Mapper.toDTO defaultTenant Nothing Nothing))
