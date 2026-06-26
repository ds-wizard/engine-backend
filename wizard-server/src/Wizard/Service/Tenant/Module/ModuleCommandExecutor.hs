module Wizard.Service.Tenant.Module.ModuleCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.Module.ModuleService
import WizardLib.Public.Model.PersistentCommand.Tenant.Module.CreateOrUpdateTenantModulesCommand

cComponent = "tenant_module"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateOrUpdateTenantModulesName = cCreateOrUpdateTenantModules command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateOrUpdateTenantModulesName = "updateRegistry"

cCreateOrUpdateTenantModules :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateOrUpdateTenantModules persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateTenantModulesCommand
  case eCommand of
    Right command -> do
      createOrUpdateTenantModules persistentCommand.tenantUuid command.modules
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
