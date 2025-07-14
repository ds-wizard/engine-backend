module Wizard.Service.Tenant.TenantCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.TenantService
import WizardLib.Public.Model.PersistentCommand.Tenant.CreateOrUpdateTenantCommand

cComponent = "tenant"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateTenantName = cCreateTenant command
  | command.function == cUpdateTenantName = cUpdateTenant command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateTenantName = "createTenant"

cCreateTenant :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateTenant persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateTenantCommand
  case eCommand of
    Right command -> do
      createTenantByCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateTenantName = "updateTenant"

cUpdateTenant :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateTenant persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateTenantCommand
  case eCommand of
    Right command -> do
      modifyTenantFromCommand command
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
