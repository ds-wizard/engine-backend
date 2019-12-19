module Wizard.Service.Version.VersionService
  ( publishPackage
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import Data.UUID as U

import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import Wizard.LensesConfig
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.BranchUtils
import Wizard.Service.Organization.OrganizationService
import Wizard.Service.Package.PackageService
import Wizard.Service.Package.PackageUtils
import Wizard.Service.Version.VersionMapper
import Wizard.Service.Version.VersionValidation

publishPackage :: String -> String -> VersionDTO -> AppContextM (Either AppError PackageSimpleDTO)
publishPackage bUuid pkgVersion reqDto =
  heFindBranchWithEventsById bUuid $ \branch -> do
    eMs <- findMigratorStateByBranchUuid bUuid
    case eMs of
      Right ms -> do
        deleteMigratorStateByBranchUuid (U.toString $ branch ^. uuid)
        doPublishPackage
          pkgVersion
          reqDto
          branch
          (ms ^. resultEvents)
          (Just $ ms ^. targetPackageId)
          (Just $ upgradePackageVersion (ms ^. branchPreviousPackageId) pkgVersion)
      Left (NotExistsError _) ->
        heGetBranchForkOfPackageId branch $ \mMergeCheckpointPkgId ->
          heGetBranchMergeCheckpointPackageId branch $ \mForkOfPkgId ->
            doPublishPackage pkgVersion reqDto branch (branch ^. events) mForkOfPkgId mMergeCheckpointPkgId
      Left error -> return . Left $ error

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage ::
     String
  -> VersionDTO
  -> BranchWithEvents
  -> [Event]
  -> Maybe String
  -> Maybe String
  -> AppContextM (Either AppError PackageSimpleDTO)
doPublishPackage pkgVersion reqDto branch events mForkOfPkgId mMergeCheckpointPkgId =
  heGetOrganization $ \org ->
    heValidateNewPackageVersion pkgVersion branch org $ do
      now <- liftIO getCurrentTime
      let pkg = fromPackage branch reqDto mForkOfPkgId mMergeCheckpointPkgId org pkgVersion events now
      createdPkg <- createPackage pkg
      let updatedBranch = fromBranch branch pkg
      updateBranchById updatedBranch
      return . Right $ createdPkg
