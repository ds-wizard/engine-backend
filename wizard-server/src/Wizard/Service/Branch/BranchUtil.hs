module Wizard.Service.Branch.BranchUtil where

import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Package.PackageService
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.Public.Model.Tenant.Config.TenantConfig

getBranchPreviousPackage :: Branch -> AppContextM (Maybe Package)
getBranchPreviousPackage branch =
  case branch.previousPackageId of
    Just pkgId -> do
      pkg <- getPackageById pkgId
      return . Just $ pkg
    Nothing -> return Nothing

getBranchForkOfPackageId :: Branch -> AppContextM (Maybe String)
getBranchForkOfPackageId branch = do
  mPreviousPkg <- getBranchPreviousPackage branch
  case mPreviousPkg of
    Just previousPkg -> do
      tenantConfig <- getCurrentTenantConfig
      let org = tenantConfig.organization
      if (previousPkg.organizationId == org.organizationId) && (previousPkg.kmId == branch.kmId)
        then return $ previousPkg.forkOfPackageId
        else return . Just $ previousPkg.pId
    Nothing -> return Nothing

getBranchMergeCheckpointPackageId :: Branch -> AppContextM (Maybe String)
getBranchMergeCheckpointPackageId branch = do
  mPreviousPkg <- getBranchPreviousPackage branch
  case mPreviousPkg of
    Just previousPkg -> do
      tenantConfig <- getCurrentTenantConfig
      let org = tenantConfig.organization
      if (previousPkg.organizationId == org.organizationId) && (previousPkg.kmId == branch.kmId)
        then return $ previousPkg.mergeCheckpointPackageId
        else return . Just $ previousPkg.pId
    Nothing -> return Nothing

getBranchState :: Branch -> Int -> Maybe String -> AppContextM BranchState
getBranchState branch eventSize mForkOfPackageId = do
  mMs <- findMigratorStateByBranchUuid' branch.uuid
  isMigrating mMs $ isEditing $ isMigrated mMs $ isOutdated isDefault
  where
    isMigrating mMs continue =
      case mMs of
        Just ms ->
          if ms.migrationState == CompletedState
            then continue
            else return BSMigrating
        Nothing -> continue
    isEditing continue =
      if eventSize > 0
        then return BSEdited
        else continue
    isMigrated mMs continue =
      case mMs of
        Just ms ->
          if ms.migrationState == CompletedState
            then return BSMigrated
            else continue
        Nothing -> continue
    isOutdated continue =
      case mForkOfPackageId of
        Just forkOfPackageId -> do
          newerPackages <- getNewerPackages forkOfPackageId False
          if not . null $ newerPackages
            then return BSOutdated
            else continue
        Nothing -> continue
    isDefault = return BSDefault
