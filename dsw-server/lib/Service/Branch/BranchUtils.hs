module Service.Branch.BranchUtils where

import Control.Lens ((^.))

import Database.DAO.Package.PackageDAO
import LensesConfig
import Service.Organization.OrganizationService
import Util.Helper (createHeeHelper)

getBranchPreviousPackage branch =
  case branch ^. previousPackageId of
    Just pkgId -> heFindPackageById pkgId $ \pkg -> return . Right . Just $ pkg
    Nothing -> return . Right $ Nothing

getBranchForkOfPackageId branch = do
  ePreviousPkg <- getBranchPreviousPackage branch
  case ePreviousPkg of
    Right (Just previousPkg) ->
      heGetOrganization $ \org ->
        if (previousPkg ^. organizationId == org ^. organizationId) && (previousPkg ^. kmId == branch ^. kmId)
          then return . Right $ previousPkg ^. forkOfPackageId
          else return . Right . Just $ previousPkg ^. pId
    Right Nothing -> return . Right $ Nothing
    Left error -> return . Left $ error

getBranchMergeCheckpointPackageId branch = do
  ePreviousPkg <- getBranchPreviousPackage branch
  case ePreviousPkg of
    Right (Just previousPkg) ->
      heGetOrganization $ \org ->
        if (previousPkg ^. organizationId == org ^. organizationId) && (previousPkg ^. kmId == branch ^. kmId)
          then return . Right $ previousPkg ^. mergeCheckpointPackageId
          else return . Right . Just $ previousPkg ^. pId
    Right Nothing -> return . Right $ Nothing
    Left error -> return . Left $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetBranchPreviousPackage branch = createHeeHelper (getBranchPreviousPackage branch)

-- -----------------------------------------------------
heGetBranchForkOfPackageId branch = createHeeHelper (getBranchForkOfPackageId branch)

-- -----------------------------------------------------
heGetBranchMergeCheckpointPackageId branch = createHeeHelper (getBranchMergeCheckpointPackageId branch)
