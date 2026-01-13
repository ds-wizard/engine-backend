module Wizard.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSimple
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Locale.LocaleList ()
import Wizard.Database.Mapping.Locale.LocaleSimple ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Locale.LocaleList

entityName = "locale"

pageLabel = "locales"

findLocales :: AppContextM [Locale]
findLocales = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleList)
findLocalesPage mOrganizationId mLocaleId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn'
    entityName
    pageLabel
    pageable
    sort
    "locale.uuid, locale.name, locale.description, locale.code, locale.organization_id, locale.locale_id, locale.version, locale.default_locale, locale.enabled, registry_locale.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, locale.created_at, locale.updated_at"
    "locale_id"
    mQuery
    Nothing
    mOrganizationId
    mLocaleId
    Nothing
    ""

findLocalesFiltered :: [(String, String)] -> AppContextM [Locale]
findLocalesFiltered queryParams = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findLocalesFilteredWithTenant :: U.UUID -> [(String, String)] -> AppContextM [Locale]
findLocalesFilteredWithTenant tenantUuid queryParams = createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findLocalesByCodeWithTenant :: U.UUID -> String -> String -> AppContextM [LocaleSimple]
findLocalesByCodeWithTenant tenantUuid code shortCode = do
  let sql =
        fromString
          "SELECT uuid, name, code, default_locale \
          \FROM locale \
          \WHERE tenant_uuid = ? \
          \  AND enabled = true \
          \  AND (code = ? \
          \    OR code = ? \
          \    OR default_locale = true);"
  let params = [toField . U.toString $ tenantUuid, toField code, toField shortCode]
  logQuery sql params
  let action conn = query conn sql params
  runDB action
