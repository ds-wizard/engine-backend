module WizardLib.Public.Database.DAO.Tenant.TenantDAO where

import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.TenantSuggestion ()
import WizardLib.Public.Model.Tenant.TenantSuggestion

entityName = "tenant"

pageLabel = "tenants"

findTenantSuggestions :: AppContextC s sc m => Maybe String -> m [TenantSuggestion]
findTenantSuggestions mQuery = do
  let sql =
        "SELECT tenant.uuid, \
        \       tenant.name, \
        \       tenant.client_url, \
        \       config_look_and_feel.primary_color, \
        \       config_look_and_feel.logo_url \
        \FROM tenant \
        \JOIN config_look_and_feel ON tenant.uuid = config_look_and_feel.tenant_uuid \
        \WHERE tenant.name ~* ? OR tenant.tenant_id ~* ? OR tenant.client_url ~* ? OR (tenant.uuid)::text ~* ?"
  let params = [regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  runDB action
