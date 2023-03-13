module Wizard.Service.Package.Publish.PackagePublishService (
  publishPackageFromBranch,
  publishPackageFromMigration,
) where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageUtil
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Acl.AclService
import Wizard.Service.Branch.BranchAudit
import Wizard.Service.Branch.BranchUtil
import Wizard.Service.Branch.Collaboration.CollaborationService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.KnowledgeModel.Squash.Squasher
import Wizard.Service.Migration.KnowledgeModel.MigratorAudit
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.Publish.PackagePublishMapper
import Wizard.Service.Package.Publish.PackagePublishValidation

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
  appConfig <- getAppConfig
  let org = appConfig.organization
  validateNewPackageVersion version branch org
  now <- liftIO getCurrentTime
  let pkg = fromPackage branch mForkOfPkgId mMergeCheckpointPkgId org version description readme squashedBranchEvents now
  createdPkg <- createPackage pkg
  let updatedBranch = branch {previousPackageId = Just pkg.pId, updatedAt = now}
  updateBranchById updatedBranch
  let updatedBranchData = branchData {events = [], updatedAt = now}
  updateBranchDataById updatedBranchData
  logOutOnlineUsersWhenBranchDramaticallyChanged branch.uuid
  return createdPkg
