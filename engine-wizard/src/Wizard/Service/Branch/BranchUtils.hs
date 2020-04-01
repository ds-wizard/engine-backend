module Wizard.Service.Branch.BranchUtils where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Package.Package
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService

getBranchPreviousPackage :: BranchWithEvents -> AppContextM (Maybe Package)
getBranchPreviousPackage branch =
  case branch ^. previousPackageId of
    Just pkgId -> do
      pkg <- findPackageById pkgId
      return . Just $ pkg
    Nothing -> return Nothing

getBranchForkOfPackageId :: BranchWithEvents -> AppContextM (Maybe String)
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

getBranchMergeCheckpointPackageId :: BranchWithEvents -> AppContextM (Maybe String)
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
