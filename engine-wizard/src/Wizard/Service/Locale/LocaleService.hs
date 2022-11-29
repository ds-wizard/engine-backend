module Wizard.Service.Locale.LocaleService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time

import Shared.Constant.Locale
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Locale.Locale
import Shared.Util.String
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.DAO.Registry.RegistryLocaleDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Locale.LocaleSimple
import Wizard.Model.Locale.LocaleState
import Wizard.S3.Locale.LocaleS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Locale.LocaleMapper
import Wizard.Service.Locale.LocaleValidation

getLocalesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page LocaleDTO)
getLocalesPage mOrganizationId mLocaleId mQuery pageable sort = do
  checkPermission _LOC_PERM
  locales <- findLocalesPage mOrganizationId mLocaleId mQuery pageable sort
  appConfig <- getAppConfig
  return . fmap (toDTO appConfig.registry.enabled) $ locales

createLocale :: LocaleCreateDTO -> BS.ByteString -> AppContextM LocaleDTO
createLocale reqDto content =
  runInTransaction $ do
    checkPermission _LOC_PERM
    now <- liftIO getCurrentTime
    appConfig <- getAppConfig
    let organizationId = appConfig.organization.organizationId
    validateLocaleCreate reqDto organizationId
    let defaultLocale = False
    let locale = fromCreateDTO reqDto organizationId defaultLocale appConfig.uuid now
    insertLocale locale
    putLocale locale.lId content
    appConfig <- getAppConfig
    return . toDTO appConfig.registry.enabled $ toLocaleList locale UnknownLocaleState

getLocaleForId :: String -> AppContextM LocaleDetailDTO
getLocaleForId lclId = do
  checkPermission _LOC_PERM
  serverConfig <- asks serverConfig
  locale <- findLocaleById lclId
  versions <- getLocaleVersions locale
  localeRs <- findRegistryLocales
  orgRs <- findRegistryOrganizations
  return $ toDetailDTO locale localeRs orgRs versions (buildLocaleUrl serverConfig.registry.clientUrl locale localeRs)

getLocaleContentForId :: String -> AppContextM BS.ByteString
getLocaleContentForId code = do
  let shortCode =
        case splitOn "-" code of
          [] -> code
          [x] -> code
          (language : _) -> language
  locales <- findLocalesByCode code shortCode
  locale <-
    case L.find (\l -> l.code == code) locales of
      Just locale -> return locale
      Nothing -> case L.find (\l -> l.code == shortCode) locales of
        Just locale -> return locale
        Nothing -> case L.find (\l -> l.defaultLocale) locales of
          Just locale -> return locale
          Nothing -> findSimpleLocaleById defaultLocaleId
  if locale.lId /= defaultLocaleId
    then getLocale locale.lId
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
    appConfig <- getAppConfig
    return . toDTO appConfig.registry.enabled $ toLocaleList updatedLocale UnknownLocaleState

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
