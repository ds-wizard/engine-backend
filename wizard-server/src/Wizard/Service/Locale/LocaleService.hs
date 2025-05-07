module Wizard.Service.Locale.LocaleService where

import Control.Monad (void, when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Time

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Locale.Constant.Locale
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSuggestion
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Registry.RegistryLocaleDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserDAO
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
import WizardLib.Public.Model.Tenant.Config.TenantConfig

getLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleDTO)
getLocalesPage mOrganizationId mLocaleId mQuery pageable sort = do
  checkPermission _LOC_PERM
  checkIfAdminIsDisabled
  locales <- findLocalesPage mOrganizationId mLocaleId mQuery pageable sort
  tenantConfig <- getCurrentTenantConfig
  return . fmap (toDTO tenantConfig.registry.enabled) $ locales

getLocaleSuggestions :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleSuggestion)
getLocaleSuggestions mQuery pageable sort = do
  checkIfAdminIsDisabled
  findLocaleSuggestions mQuery pageable sort

createLocale :: LocaleCreateDTO -> AppContextM LocaleDTO
createLocale reqDto =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    checkLocaleLimit
    now <- liftIO getCurrentTime
    tenantConfig <- getCurrentTenantConfig
    let organizationId = tenantConfig.organization.organizationId
    validateLocaleCreate reqDto organizationId
    let defaultLocale = False
    let locale = fromCreateDTO reqDto organizationId defaultLocale tenantConfig.uuid now
    insertLocale locale
    putLocale locale.lId "wizard.json" reqDto.wizardContent
    putLocale locale.lId "mail.po" reqDto.mailContent
    tenantConfig <- getCurrentTenantConfig
    return . toDTO tenantConfig.registry.enabled $ toLocaleList locale

getLocaleForId :: String -> AppContextM LocaleDetailDTO
getLocaleForId lclId = do
  checkPermission _LOC_PERM
  checkIfAdminIsDisabled
  serverConfig <- asks serverConfig
  locale <- findLocaleById lclId
  versions <- getLocaleVersions locale
  localeRs <- findRegistryLocales
  orgRs <- findRegistryOrganizations
  tenantConfig <- getCurrentTenantConfig
  return $ toDetailDTO locale tenantConfig.registry.enabled localeRs orgRs versions (buildLocaleUrl serverConfig.registry.clientUrl locale localeRs)

getLocaleContentForCurrentUser :: Maybe String -> AppContextM BS.ByteString
getLocaleContentForCurrentUser mClientUrl = do
  checkIfAdminIsDisabled
  tenant <- maybe getCurrentTenant findTenantByClientUrl mClientUrl
  mUser <- asks currentUser
  locale <-
    case mUser of
      Just user ->
        case user.locale of
          Just lclId -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("id", lclId)]
          Nothing -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("default_locale", show True)]
      Nothing -> findLocaleSuggestionBy [tenantQueryUuid tenant.uuid, ("default_locale", show True)]
  if locale.lId /= defaultLocaleId
    then retrieveLocaleWithTenant tenant.uuid locale.lId "wizard.json"
    else return "{}"

modifyLocale :: String -> LocaleChangeDTO -> AppContextM LocaleDTO
modifyLocale lclId reqDto = do
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    now <- liftIO getCurrentTime
    locale <- findLocaleById lclId
    validateLocaleChange reqDto locale
    let updatedLocale = fromChangeDTO locale reqDto now
    when (updatedLocale.defaultLocale && not locale.defaultLocale) unsetDefaultLocale
    when (updatedLocale.enabled && not locale.enabled) (unsetEnabledLocale updatedLocale.code)
    updateLocaleById updatedLocale
    tenantConfig <- getCurrentTenantConfig
    return . toDTO tenantConfig.registry.enabled $ toLocaleList updatedLocale

deleteLocalesByQueryParams :: [(String, String)] -> AppContextM ()
deleteLocalesByQueryParams queryParams =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    locales <- findLocalesFiltered queryParams
    traverse_ validateLocaleDeletation locales
    traverse_ (deleteLocale . (.lId)) locales

deleteLocale :: String -> AppContextM ()
deleteLocale lclId =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkIfAdminIsDisabled
    locale <- findLocaleById lclId
    validateLocaleDeletation locale
    users <- findUsersFiltered [("locale", locale.lId)]
    traverse_ (\u -> unsetUserLocale u.uuid) users
    void $ deleteLocaleById lclId

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: Locale -> AppContextM [String]
getLocaleVersions locale = do
  allLocales <- findLocalesByOrganizationIdAndLocaleId locale.organizationId locale.localeId
  return . fmap (.version) $ allLocales

checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "Locale Endpoints" (\s -> not s.admin.enabled)
