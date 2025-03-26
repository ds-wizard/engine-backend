module Wizard.Service.Locale.LocaleService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.String
import Shared.Locale.Constant.Locale
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Registry.RegistryLocaleDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Locale.LocaleSimple
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.S3.Locale.LocaleS3
import Wizard.Service.Locale.LocaleMapper
import Wizard.Service.Locale.LocaleValidation
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantHelper
import WizardLib.Public.Model.Tenant.Config.TenantConfig

getLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleDTO)
getLocalesPage mOrganizationId mLocaleId mQuery pageable sort = do
  checkPermission _LOC_PERM
  locales <- findLocalesPage mOrganizationId mLocaleId mQuery pageable sort
  tenantConfig <- getCurrentTenantConfig
  return . fmap (toDTO tenantConfig.registry.enabled) $ locales

createLocale :: LocaleCreateDTO -> AppContextM LocaleDTO
createLocale reqDto =
  runInTransaction $ do
    checkPermission _LOC_PERM
    checkLocaleLimit
    now <- liftIO getCurrentTime
    tenantConfig <- getCurrentTenantConfig
    let organizationId = tenantConfig.organization.organizationId
    validateLocaleCreate reqDto organizationId
    let defaultLocale = False
    let locale = fromCreateDTO reqDto organizationId defaultLocale tenantConfig.uuid now
    insertLocale locale
    putLocale locale.lId reqDto.content
    tenantConfig <- getCurrentTenantConfig
    return . toDTO tenantConfig.registry.enabled $ toLocaleList locale

getLocaleForId :: String -> AppContextM LocaleDetailDTO
getLocaleForId lclId = do
  checkPermission _LOC_PERM
  serverConfig <- asks serverConfig
  locale <- findLocaleById lclId
  versions <- getLocaleVersions locale
  localeRs <- findRegistryLocales
  orgRs <- findRegistryOrganizations
  tenantConfig <- getCurrentTenantConfig
  return $ toDetailDTO locale tenantConfig.registry.enabled localeRs orgRs versions (buildLocaleUrl serverConfig.registry.clientUrl locale localeRs)

getLocaleContentForId :: String -> Maybe String -> AppContextM BS.ByteString
getLocaleContentForId code mClientUrl = do
  serverConfig <- asks serverConfig
  tenant <-
    if serverConfig.cloud.enabled
      then maybe getCurrentTenant findTenantByClientUrl mClientUrl
      else getCurrentTenant
  let shortCode =
        case splitOn "-" code of
          [] -> code
          [x] -> code
          (language : _) -> language
  locales <- findLocalesByCodeWithTenant tenant.uuid code shortCode
  locale <-
    case L.find (\l -> l.code == code) locales of
      Just locale -> return locale
      Nothing -> case L.find (\l -> l.code == shortCode) locales of
        Just locale -> return locale
        Nothing -> case L.find (\l -> l.defaultLocale) locales of
          Just locale -> return locale
          Nothing -> findSimpleLocaleByIdWithTenant tenant.uuid defaultLocaleId
  if locale.lId /= defaultLocaleId
    then retrieveLocaleWithTenant tenant.uuid locale.lId
    else return "{}"

modifyLocale :: String -> LocaleChangeDTO -> AppContextM LocaleDTO
modifyLocale lclId reqDto = do
  runInTransaction $ do
    checkPermission _LOC_PERM
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
    locales <- findLocalesFiltered queryParams
    traverse_ validateLocaleDeletation locales
    traverse_ (deleteLocale . (.lId)) locales

deleteLocale :: String -> AppContextM ()
deleteLocale lclId =
  runInTransaction $ do
    checkPermission _LOC_PERM
    locale <- findLocaleById lclId
    validateLocaleDeletation locale
    deleteLocaleById lclId
    removeLocale locale.lId
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: Locale -> AppContextM [String]
getLocaleVersions locale = do
  allLocales <- findLocalesByOrganizationIdAndLocaleId locale.organizationId locale.localeId
  return . fmap (.version) $ allLocales
