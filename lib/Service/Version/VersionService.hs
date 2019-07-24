module Service.Version.VersionService
  ( publishPackage
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import Data.UUID as U

import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Version.VersionDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Migration.KnowledgeModel.MigratorDAO
import LensesConfig
import Model.Branch.Branch
import Model.Context.AppContext
import Model.Error.Error
import Model.Event.Event
import Service.Branch.BranchUtils
import Service.Organization.OrganizationService
import Service.Package.PackageService
import Service.Version.VersionMapper
import Service.Version.VersionValidation

publishPackage :: String -> String -> VersionDTO -> AppContextM (Either AppError PackageSimpleDTO)
publishPackage bUuid pkgVersion reqDto =
  heFindBranchWithEventsById bUuid $ \branch -> do
    eMs <- findMigratorStateByBranchUuid bUuid
    case eMs of
      Right ms -> do
        deleteMigratorStateByBranchUuid (U.toString $ branch ^. uuid)
        doPublishPackage pkgVersion reqDto branch (ms ^. resultEvents)
      Left (NotExistsError _) -> doPublishPackage pkgVersion reqDto branch (branch ^. events)
      Left error -> return . Left $ error

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage ::
     String -> VersionDTO -> BranchWithEvents -> [Event] -> AppContextM (Either AppError PackageSimpleDTO)
doPublishPackage pkgVersion reqDto branch events =
  heGetBranchForkOfPackageId branch $ \mMergeCheckpointPkgId ->
    heGetBranchMergeCheckpointPackageId branch $ \mForkOfPkgId ->
      heGetOrganization $ \org ->
        heValidateNewPackageVersion pkgVersion branch org $ do
          now <- liftIO getCurrentTime
          let pkg = fromPackage branch reqDto mForkOfPkgId mMergeCheckpointPkgId org pkgVersion events now
          createdPkg <- createPackage pkg
          let updatedBranch = fromBranch branch pkg
          updateBranchById updatedBranch
          return . Right $ createdPkg
