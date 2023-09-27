module Wizard.Service.Tenant.Config.ConfigCommandExecutor where

import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.PersistentCommand.Config.InvokeClientCssCompilationCommand
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.CreateAuthenticationConfigCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand

cComponent = "TenantConfig"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cInvokeClientCssCompilationName = cInvokeClientCssCompilation command
  | command.function == cCreateAuthenticationName = cCreateAuthentication command
  | command.function == cUpdateRegistryName = cUpdateRegistry command

recompileCssInAllTenants :: AppContextM ()
recompileCssInAllTenants =
  runInTransaction $ do
    tenants <- findTenants
    traverse_ (\a -> recompileCssInTenant a.uuid) tenants

recompileCssInTenant :: U.UUID -> AppContextM ()
recompileCssInTenant tenantUuid = do
  pUuid <- liftIO generateUuid
  user <- getCurrentUser
  now <- liftIO getCurrentTime
  let command =
        toPersistentCommand
          pUuid
          cComponent
          cInvokeClientCssCompilationName
          (BSL.unpack . encode $ InvokeClientCssCompilationCommand tenantUuid)
          1
          True
          Nothing
          tenantUuid
          (Just . U.toString $ user.uuid)
          now
  insertPersistentCommand command
  return ()

cInvokeClientCssCompilationName = "invokeClientCssCompilation"

cInvokeClientCssCompilation :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cInvokeClientCssCompilation persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String InvokeClientCssCompilationCommand
  case eCommand of
    Right command -> do
      tenantConfig <- getTenantConfigByUuid command.tenantUuid
      updatedTenantConfig <- invokeClientCssCompilation tenantConfig tenantConfig
      modifyTenantConfig updatedTenantConfig
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

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
