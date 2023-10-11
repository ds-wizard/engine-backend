module Wizard.Service.Tenant.Config.ConfigCommandExecutor where

import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateLookAndFeelConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand

cComponent = "TenantConfig"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateAuthenticationName = cCreateAuthentication command
  | command.function == cUpdateRegistryName = cUpdateRegistry command
  | command.function == cUpdateLookAndFeelName = cUpdateLookAndFeel command

cCreateAuthenticationName = "createAuthentication"

cCreateAuthentication :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateAuthentication persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateAuthenticationConfigCommand
  case eCommand of
    Right command -> do
      tenantConfig <- getTenantConfigByUuid command.tenantUuid
      now <- liftIO getCurrentTime
      let updatedTenantConfig = fromAuthenticationCommand tenantConfig command now
      modifyTenantConfig updatedTenantConfig
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateRegistryName = "updateRegistry"

cUpdateRegistry :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateRegistry persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateRegistryConfigCommand
  case eCommand of
    Right command -> do
      tenantConfig <- getTenantConfigByUuid command.tenantUuid
      now <- liftIO getCurrentTime
      let updatedTenantConfig = fromRegistry tenantConfig command now
      modifyTenantConfig updatedTenantConfig
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

cUpdateLookAndFeelName = "updateLookAndFeel"

cUpdateLookAndFeel :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cUpdateLookAndFeel persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String UpdateLookAndFeelConfigCommand
  case eCommand of
    Right command -> do
      tenantConfig <- getTenantConfigByUuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      let updatedTenantConfig = fromLookAndFeel tenantConfig command now
      modifyTenantConfig updatedTenantConfig
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
