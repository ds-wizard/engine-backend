module Wizard.Service.Branch.BranchUtil where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Package.Package
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchState
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchMapper
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Package.PackageService

getBranchPreviousPackage :: Branch -> AppContextM (Maybe Package)
getBranchPreviousPackage branch =
  case branch ^. previousPackageId of
    Just pkgId -> do
      pkg <- getPackageById pkgId
      return . Just $ pkg
    Nothing -> return Nothing

getBranchForkOfPackageId :: Branch -> AppContextM (Maybe String)
getBranchForkOfPackageId branch = do
  mPreviousPkg <- getBranchPreviousPackage branch
  case mPreviousPkg of
    Just previousPkg -> do
      appConfig <- getAppConfig
      let org = appConfig ^. organization
      if (previousPkg ^. organizationId == org ^. organizationId) && (previousPkg ^. kmId == branch ^. kmId)
        then return $ previousPkg ^. forkOfPackageId
        else return . Just $ previousPkg ^. pId
    Nothing -> return Nothing

getBranchMergeCheckpointPackageId :: Branch -> AppContextM (Maybe String)
getBranchMergeCheckpointPackageId branch = do
  mPreviousPkg <- getBranchPreviousPackage branch
  case mPreviousPkg of
    Just previousPkg -> do
      appConfig <- getAppConfig
      let org = appConfig ^. organization
      if (previousPkg ^. organizationId == org ^. organizationId) && (previousPkg ^. kmId == branch ^. kmId)
        then return $ previousPkg ^. mergeCheckpointPackageId
        else return . Just $ previousPkg ^. pId
    Nothing -> return Nothing

enhanceBranch :: Branch -> AppContextM BranchDTO
enhanceBranch branch = do
  mForkOfPackageId <- getBranchForkOfPackageId branch
  branchData <- findBranchDataById (U.toString $ branch ^. uuid)
  state <- getBranchState branch branchData
  return $ toDTO branch mForkOfPackageId state

getBranchState :: Branch -> BranchData -> AppContextM BranchState
getBranchState branch branchData = isMigrating $ isEditing $ isMigrated $ isOutdated isDefault
  where
    isMigrating continue = do
      mMs <- findMigratorStateByBranchUuid' (U.toString $ branch ^. uuid)
      case mMs of
        Just ms ->
          if ms ^. migrationState == CompletedState
            then continue
            else return BSMigrating
        Nothing -> continue
    isEditing continue =
      if not (null $ branchData ^. events)
        then return BSEdited
        else continue
    isMigrated continue = do
      mMs <- findMigratorStateByBranchUuid' (U.toString $ branch ^. uuid)
      case mMs of
        Just ms ->
          if ms ^. migrationState == CompletedState
            then return BSMigrated
            else continue
        Nothing -> continue
    isOutdated continue = do
      mForkOfPackageId <- getBranchForkOfPackageId branch
      case mForkOfPackageId of
        Just forkOfPackageId -> do
          newerPackages <- getNewerPackages forkOfPackageId
          if not . null $ newerPackages
            then return BSOutdated
            else continue
        Nothing -> continue
    isDefault = return BSDefault
