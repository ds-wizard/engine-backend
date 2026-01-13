module Wizard.Service.Locale.LocaleService where

import Control.Monad (void, when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Uuid
import Shared.Locale.Constant.Locale
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSimple
import Shared.Locale.Model.Locale.LocaleSuggestion
import Shared.Locale.Service.Locale.LocaleMapper
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Registry.RegistryLocaleDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.S3.Locale.LocaleS3
import Wizard.Service.Common
import Wizard.Service.Locale.LocaleMapper
import Wizard.Service.Locale.LocaleValidation
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantHelper

getLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleDTO)
getLocalesPage mOrganizationId mLocaleId mQuery pageable sort = do
  checkPermission _LOC_PERM
  checkIfAdminIsDisabled
  locales <- findLocalesPage mOrganizationId mLocaleId mQuery pageable sort
  tcRegistry <- getCurrentTenantConfigRegistry
  return . fmap (toDTO tcRegistry.enabled) $ locales

getLocaleSuggestions :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleSuggestion)
getLocaleSuggestions mQuery pageable sort = do
  checkIfAdminIsDisabled
  findLocaleSuggestions mQuery pageable sort

createLocale :: LocaleCreateDTO -> AppContextM LocaleSimple
createLocale reqDto =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    checkLocaleLimit
    now <- liftIO getCurrentTime
    uuid <- liftIO generateUuid
    tcOrganization <- findTenantConfigOrganization
    let organizationId = tcOrganization.organizationId
    validateLocaleCreate reqDto organizationId
    let defaultLocale = False
    let locale = fromCreateDTO reqDto uuid organizationId defaultLocale tcOrganization.tenantUuid now
    insertLocale locale
    putLocale locale.uuid "wizard.json" reqDto.wizardContent
    putLocale locale.uuid "mail.po" reqDto.mailContent
    tcRegistry <- getCurrentTenantConfigRegistry
    return . toSimple $ locale

getLocaleByUuid :: U.UUID -> AppContextM LocaleDetailDTO
getLocaleByUuid uuid = do
  checkPermission _LOC_PERM
  checkIfAdminIsDisabled
  serverConfig <- asks serverConfig
  locale <- findLocaleByUuid uuid
  versions <- getLocaleVersions locale
  localeRs <- findRegistryLocales
  orgRs <- findRegistryOrganizations
  tcRegistry <- getCurrentTenantConfigRegistry
  return $ toDetailDTO locale tcRegistry.enabled localeRs orgRs versions (buildLocaleUrl serverConfig.registry.clientUrl locale localeRs)

getLocaleContentForCurrentUser :: Maybe String -> AppContextM BS.ByteString
getLocaleContentForCurrentUser mClientUrl = do
  checkIfAdminIsDisabled
  tenant <- maybe getCurrentTenant findTenantByClientUrl mClientUrl
  mUser <- asks currentUser
  locale <-
    case mUser of
      Just user ->
        case user.locale of
          Just localeUuid -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("uuid", U.toString localeUuid)]
          Nothing -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("default_locale", show True)]
      Nothing -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("default_locale", show True)]
  if not (locale.organizationId == defaultLocaleOrganizationId && locale.localeId == defaultLocaleLocaleId && locale.version == defaultLocaleVersion)
    then retrieveLocaleWithTenant tenant.uuid locale.uuid "wizard.json"
    else return "{}"

modifyLocale :: U.UUID -> LocaleChangeDTO -> AppContextM LocaleDTO
modifyLocale uuid reqDto = do
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    now <- liftIO getCurrentTime
    locale <- findLocaleByUuid uuid
    validateLocaleChange reqDto locale
    let updatedLocale = fromChangeDTO locale reqDto now
    when (updatedLocale.defaultLocale && not locale.defaultLocale) unsetDefaultLocale
    when (updatedLocale.enabled && not locale.enabled) (unsetEnabledLocale updatedLocale.code)
    updateLocaleByUuid updatedLocale
    tcRegistry <- getCurrentTenantConfigRegistry
    return . toDTO tcRegistry.enabled $ toLocaleList updatedLocale

deleteLocalesByQueryParams :: [(String, String)] -> AppContextM ()
deleteLocalesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    locales <- findLocalesFiltered queryParams
    traverse_ validateLocaleDeletion locales
    traverse_ (deleteLocaleByUuid . (.uuid)) locales

deleteLocale :: U.UUID -> AppContextM ()
deleteLocale uuid =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    locale <- findLocaleByUuid uuid
    validateLocaleDeletion locale
    void $ deleteLocaleByUuid locale.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: Locale -> AppContextM [String]
getLocaleVersions locale = do
  allLocales <- findLocalesByOrganizationIdAndLocaleId locale.organizationId locale.localeId
  return . fmap (.version) $ allLocales

checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "Locale Endpoints" (\s -> not s.admin.enabled)
