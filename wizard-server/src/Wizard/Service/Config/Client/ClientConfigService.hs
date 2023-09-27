module Wizard.Service.Config.Client.ClientConfigService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Config.Client.ClientConfigMapper
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper

getClientConfig :: Maybe String -> AppContextM ClientConfigDTO
getClientConfig mClientUrl = do
  serverConfig <- asks serverConfig
  tenant <-
    if serverConfig.cloud.enabled
      then maybe getCurrentTenant findTenantByClientUrl mClientUrl
      else getCurrentTenant
  unless tenant.enabled (throwError LockedError)
  tenantConfig <-
    if serverConfig.cloud.enabled
      then case mClientUrl of
        Just clientUrl -> getTenantConfigByUuid tenant.uuid
        Nothing -> getCurrentTenantConfig
      else getCurrentTenantConfig
  locales <- findLocalesFilteredWithTenant tenant.uuid [("enabled", show True)]
  return $ toClientConfigDTO serverConfig tenantConfig tenant locales
