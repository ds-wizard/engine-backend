module Wizard.Service.Package.Publish.PackagePublishService (
  publishPackageFromBranch,
  publishPackageFromMigration,
) where

import Control.Monad.Reader (liftIO)
import Data.Time

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Branch.BranchAudit
import Wizard.Service.Branch.BranchUtil
import Wizard.Service.Branch.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.Squash.Squasher
import Wizard.Service.Migration.KnowledgeModel.MigratorAudit
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.Publish.PackagePublishMapper
import Wizard.Service.Package.Publish.PackagePublishValidation
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

publishPackageFromBranch :: PackagePublishBranchDTO -> AppContextM PackageSimpleDTO
publishPackageFromBranch reqDto = do
  runInTransaction $ do
    checkPermission _KM_PUBLISH_PERM
    validateMigrationExistence reqDto.branchUuid
    branch <- findBranchByUuid reqDto.branchUuid
    branchData <- findBranchDataById reqDto.branchUuid
    mMergeCheckpointPkgId <- getBranchForkOfPackageId branch
    mForkOfPkgId <- getBranchMergeCheckpointPackageId branch
    auditBranchPublish branch branchData mForkOfPkgId
    doPublishPackage
      branch.version
      branch
      branchData
      branchData.events
      branch.description
      branch.readme
      mForkOfPkgId
      mMergeCheckpointPkgId

publishPackageFromMigration :: PackagePublishMigrationDTO -> AppContextM PackageSimpleDTO
publishPackageFromMigration reqDto = do
  runInTransaction $ do
    checkPermission _KM_PUBLISH_PERM
    branch <- findBranchByUuid reqDto.branchUuid
    branchData <- findBranchDataById reqDto.branchUuid
    ms <- findMigratorStateByBranchUuid reqDto.branchUuid
    deleteMigratorStateByBranchUuid reqDto.branchUuid
    auditKmMigrationFinish reqDto.branchUuid
    doPublishPackage
      reqDto.version
      branch
      branchData
      ms.resultEvents
      reqDto.description
      reqDto.readme
      (Just ms.targetPackageId)
      (Just $ upgradePackageVersion ms.branchPreviousPackageId reqDto.version)

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage
  :: String
  -> Branch
  -> BranchData
  -> [Event]
  -> String
  -> String
  -> Maybe String
  -> Maybe String
  -> AppContextM PackageSimpleDTO
doPublishPackage version branch branchData branchEvents description readme mForkOfPkgId mMergeCheckpointPkgId = do
  let squashedBranchEvents = squash branchEvents
  tcOrganization <- findTenantConfigOrganization
  validateNewPackageVersion version branch tcOrganization
  now <- liftIO getCurrentTime
  let pkg = fromPackage branch mForkOfPkgId mMergeCheckpointPkgId tcOrganization version description readme squashedBranchEvents now
  createdPkg <- createPackage pkg
  let updatedBranch = branch {previousPackageId = Just pkg.pId, updatedAt = now} :: Branch
  updateBranchById updatedBranch
  let updatedBranchData = branchData {events = [], updatedAt = now}
  updateBranchDataById updatedBranchData
  logOutOnlineUsersWhenBranchDramaticallyChanged branch.uuid
  return createdPkg
