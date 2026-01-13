module Wizard.Service.Registry.Synchronization.RegistrySynchronizationService where

import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Time

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Wizard.Database.DAO.Registry.RegistryLocaleDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Common
import Wizard.Service.Registry.RegistryMapper
import Wizard.Service.Registry.RegistryUtil
import Wizard.Service.Statistics.StatisticsService

synchronizeData :: AppContextM ()
synchronizeData = do
  checkIfRegistryIsEnabled
  now <- liftIO getCurrentTime
  synchronizeOrganizations now
  synchronizePackages now
  synchronizeTemplates now
  synchronizeLocales now

synchronizeOrganizations :: UTCTime -> AppContextM ()
synchronizeOrganizations now = do
  logInfoI _CMP_SERVICE "Organization Synchronization started"
  organizations <- retrieveOrganizations
  let orgsRemote = fmap (`toRegistryOrganization` now) organizations
  runInTransaction $ do
    orgsLocal <- findRegistryOrganizations
    -- 1. Delete the old one
    let deletedOrganizations = getDiffOrganizations orgsLocal orgsRemote
    if null deletedOrganizations
      then logInfoI _CMP_SERVICE "No outdated organizations"
      else do
        let organizationIds = fmap (.organizationId) deletedOrganizations
        deleteRegistryOrganizationsByOrganizationIds organizationIds
    -- 2. Insert the new one
    let newOrganizations = getDiffOrganizations orgsRemote orgsLocal
    if null newOrganizations
      then logInfoI _CMP_SERVICE "No new organizations"
      else do
        let organizationIds = fmap (.organizationId) newOrganizations
        traverse_ insertRegistryOrganization newOrganizations
    logInfoI _CMP_SERVICE "Organization Synchronization successfully finished"

synchronizePackages :: UTCTime -> AppContextM ()
synchronizePackages now = do
  logInfoI _CMP_SERVICE "Package Synchronization started"
  iStat <- getInstanceStatistics
  packages <- retrievePackages iStat
  let registryPackages = fmap (`toRegistryPackage` now) packages
  deleteRegistryPackages
  traverse_ insertRegistryPackage registryPackages
  logInfoI _CMP_SERVICE "Organization Synchronization successfully finished"

synchronizeTemplates :: UTCTime -> AppContextM ()
synchronizeTemplates now = do
  logInfoI _CMP_SERVICE "DocumentTemplate Synchronization started"
  templates <- retrieveTemplates
  let registryTemplates = fmap (`toRegistryTemplate` now) templates
  deleteRegistryTemplates
  traverse_ insertRegistryTemplate registryTemplates
  logInfoI _CMP_SERVICE "Organization Synchronization successfully finished"

synchronizeLocales :: UTCTime -> AppContextM ()
synchronizeLocales now = do
  logInfoI _CMP_SERVICE "Locale Synchronization started"
  templates <- retrieveLocales
  let registryLocales = fmap (`toRegistryLocale` now) templates
  deleteRegistryLocales
  traverse_ insertRegistryLocale registryLocales
  logInfoI _CMP_SERVICE "Organization Synchronization successfully finished"

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfRegistryIsEnabled = checkIfTenantFeatureIsEnabled "Registry" findTenantConfigRegistry (.enabled)
