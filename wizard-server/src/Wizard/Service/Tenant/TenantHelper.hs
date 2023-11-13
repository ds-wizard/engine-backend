module Wizard.Service.Tenant.TenantHelper where

import Control.Monad.Reader (asks)

import Shared.Common.Model.Config.ServerConfig
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant

getCurrentTenant :: AppContextM Tenant
getCurrentTenant = do
  tntUuid <- asks currentTenantUuid
  findTenantByUuid tntUuid

getClientUrl :: AppContextM String
getClientUrl = do
  serverConfig <- asks serverConfig
  if serverConfig.cloud.enabled
    then do
      tenant <- getCurrentTenant
      return tenant.clientUrl
    else return serverConfig.general.clientUrl
