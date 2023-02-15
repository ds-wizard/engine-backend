module Wizard.Service.Registry.RegistryService where

import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time

import Registry.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Registry.RegistryLocaleDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Common
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService
import Wizard.Service.LocaleBundle.LocaleBundleService
import Wizard.Service.PackageBundle.PackageBundleService
import Wizard.Service.Registry.RegistryMapper
import Wizard.Service.Statistics.StatisticsService
import Wizard.Util.Logger

signUpToRegistry :: RegistryCreateDTO -> AppContextM OrganizationDTO
signUpToRegistry reqDto =
  runInTransaction $ do
    appConfig <- getAppConfig
    let orgCreateDto = toOrganizationCreate appConfig reqDto
    createOrganization orgCreateDto

confirmRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmRegistration reqDto =
  runInTransaction $ do
    org <- confirmOrganizationRegistration reqDto
    appConfig <- getAppConfig
    let updatedRegistry = AppConfigRegistry {enabled = True, token = org.token}
    let updatedAppConfig = appConfig {registry = updatedRegistry}
    modifyAppConfig updatedAppConfig
    return org

synchronizeData :: AppContextM ()
synchronizeData = do
  runInTransaction $ do
    checkIfRegistryIsEnabled
    now <- liftIO getCurrentTime
    synchronizeOrganizations now
    synchronizePackages now
    synchronizeTemplates now
    synchronizeLocales now

synchronizeOrganizations :: UTCTime -> AppContextM ()
synchronizeOrganizations now = do
  logInfoU _CMP_SERVICE "Organization Synchronization started"
  organizations <- retrieveOrganizations
  let registryOrganizations = fmap (`toRegistryOrganization` now) organizations
  deleteRegistryOrganizations
  traverse_ insertRegistryOrganization registryOrganizations
  logInfoU _CMP_SERVICE "Organization Synchronization successfully finished"

synchronizePackages :: UTCTime -> AppContextM ()
synchronizePackages now = do
  logInfoU _CMP_SERVICE "Package Synchronization started"
  iStat <- getInstanceStatistics
  packages <- retrievePackages iStat
  let registryPackages = fmap (`toRegistryPackage` now) packages
  deleteRegistryPackages
  traverse_ insertRegistryPackage registryPackages
  logInfoU _CMP_SERVICE "Organization Synchronization successfully finished"

synchronizeTemplates :: UTCTime -> AppContextM ()
synchronizeTemplates now = do
  logInfoU _CMP_SERVICE "DocumentTemplate Synchronization started"
  templates <- retrieveTemplates
  let registryTemplates = fmap (`toRegistryTemplate` now) templates
  deleteRegistryTemplates
  traverse_ insertRegistryTemplate registryTemplates
  logInfoU _CMP_SERVICE "Organization Synchronization successfully finished"

synchronizeLocales :: UTCTime -> AppContextM ()
synchronizeLocales now = do
  logInfoU _CMP_SERVICE "Locale Synchronization started"
  templates <- retrieveLocales
  let registryLocales = fmap (`toRegistryLocale` now) templates
  deleteRegistryLocales
  traverse_ insertRegistryLocale registryLocales
  logInfoU _CMP_SERVICE "Organization Synchronization successfully finished"

pushPackageBundle :: String -> AppContextM ()
pushPackageBundle pkgId = do
  logInfoU _CMP_SERVICE (f' "Pushing package bundle with the id ('%s') to registry" [pkgId])
  bundle <- exportPackageBundle pkgId
  uploadPackageBundle bundle
  logInfoU _CMP_SERVICE (f' "Pushing package bundle with the id ('%s') successfully completed" [pkgId])

pushDocumentTemplateBundle :: String -> AppContextM ()
pushDocumentTemplateBundle tmlId = do
  logInfoU _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') to registry" [tmlId])
  bundle <- exportBundle tmlId
  uploadDocumentTemplateBundle bundle
  logInfoU _CMP_SERVICE (f' "Pushing document template bundle with the id ('%s') successfully completed" [tmlId])

pushLocaleBundle :: String -> AppContextM ()
pushLocaleBundle lclId = do
  logInfoU _CMP_SERVICE (f' "Pushing locale bundle with the id ('%s') to registry" [lclId])
  bundle <- exportLocaleBundle lclId
  uploadLocaleBundle bundle
  logInfoU _CMP_SERVICE (f' "Pushing locale bundle with the id ('%s') successfully completed" [lclId])

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfRegistryIsEnabled = checkIfAppFeatureIsEnabled "Registry" (\c -> c.registry.enabled)
