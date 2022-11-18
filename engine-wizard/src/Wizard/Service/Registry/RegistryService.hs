module Wizard.Service.Registry.RegistryService where

import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time

import Registry.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Common
import Wizard.Service.Config.AppConfigService
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
  logInfoU _CMP_SERVICE "Template Synchronization started"
  templates <- retrieveTemplates
  let registryTemplates = fmap (`toRegistryTemplate` now) templates
  deleteRegistryTemplates
  traverse_ insertRegistryTemplate registryTemplates
  logInfoU _CMP_SERVICE "Organization Synchronization successfully finished"

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfRegistryIsEnabled = checkIfAppFeatureIsEnabled "Registry" (\c -> c.registry.enabled)
