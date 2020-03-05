module Wizard.Service.Version.VersionService
  ( publishPackage
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import Data.UUID as U

import LensesConfig
import Shared.Model.Event.Event
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchUtils
import Wizard.Service.Organization.OrganizationService
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Version.VersionMapper
import Wizard.Service.Version.VersionValidation

publishPackage :: String -> String -> VersionDTO -> AppContextM PackageSimpleDTO
publishPackage bUuid pkgVersion reqDto = do
  branch <- findBranchWithEventsById bUuid
  mMs <- findMigratorStateByBranchUuid' bUuid
  case mMs of
    Just ms -> do
      deleteMigratorStateByBranchUuid (U.toString $ branch ^. uuid)
      doPublishPackage
        pkgVersion
        reqDto
        branch
        (ms ^. resultEvents)
        (Just $ ms ^. targetPackageId)
        (Just $ upgradePackageVersion (ms ^. branchPreviousPackageId) pkgVersion)
    Nothing -> do
      mMergeCheckpointPkgId <- getBranchForkOfPackageId branch
      mForkOfPkgId <- getBranchMergeCheckpointPackageId branch
      doPublishPackage pkgVersion reqDto branch (branch ^. events) mForkOfPkgId mMergeCheckpointPkgId

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage ::
     String -> VersionDTO -> BranchWithEvents -> [Event] -> Maybe String -> Maybe String -> AppContextM PackageSimpleDTO
doPublishPackage pkgVersion reqDto branch events mForkOfPkgId mMergeCheckpointPkgId = do
  org <- getOrganization
  validateNewPackageVersion pkgVersion branch org
  now <- liftIO getCurrentTime
  let pkg = fromPackage branch reqDto mForkOfPkgId mMergeCheckpointPkgId org pkgVersion events now
  createdPkg <- createPackage pkg
  let updatedBranch = fromBranch branch pkg
  updateBranchById updatedBranch
  return createdPkg
