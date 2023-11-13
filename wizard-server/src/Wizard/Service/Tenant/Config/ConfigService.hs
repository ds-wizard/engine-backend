module Wizard.Service.Tenant.Config.ConfigService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigEM ()
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigValidation

getCurrentTenantConfig :: AppContextM TenantConfig
getCurrentTenantConfig = do
  serverConfig <- asks serverConfig
  encryptedTenantConfig <- findCurrentTenantConfig
  return $ process serverConfig.general.secret encryptedTenantConfig

getTenantConfigByUuid :: U.UUID -> AppContextM TenantConfig
getTenantConfigByUuid tenantUuid = do
  serverConfig <- asks serverConfig
  encryptedTenantConfig <- findCurrentTenantConfigByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTenantConfig

getCurrentTenantConfigDto :: AppContextM TenantConfig
getCurrentTenantConfigDto = do
  checkPermission _CFG_PERM
  getCurrentTenantConfig

modifyTenantConfig :: TenantConfig -> AppContextM TenantConfig
modifyTenantConfig tenantConfig =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedTenantConfig = process serverConfig.general.secret tenantConfig
    updateTenantConfig encryptedUpdatedTenantConfig
    return tenantConfig

modifyTenantConfigDto :: TenantConfigChangeDTO -> AppContextM TenantConfig
modifyTenantConfigDto reqDto =
  runInTransaction $ do
    checkPermission _CFG_PERM
    tenantConfig <- getCurrentTenantConfig
    validateTenantConfig reqDto
    now <- liftIO getCurrentTime
    let updatedTenantConfig = fromChangeDTO reqDto tenantConfig now
    modifyTenantConfig updatedTenantConfig
    return updatedTenantConfig
