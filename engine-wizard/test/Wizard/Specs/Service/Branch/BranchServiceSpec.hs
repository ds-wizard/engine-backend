module Wizard.Specs.Service.Branch.BranchServiceSpec where

import Control.Lens
import Control.Monad.Reader
import Data.Either
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Database.DAO.Package.PackageDAO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Branch.Data.Branches
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Model.Branch.BranchState
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchService
import Wizard.Service.Migration.KnowledgeModel.MigratorService

import Wizard.Specs.Common

branchServiceIntegrationSpec appContext =
  describe "Branch Service Integration" $ do
    let branchUuid = U.toString $ amsterdamBranchWithEvents ^. uuid
    describe "getBranchState" $ do
      it "BSDefault - no edit events, no new parent package version" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (deletePackageById (netherlandsPackageV2 ^. pId)) appContext
        -- AND: Prepare branch
        let branch = amsterdamBranchWithEvents & events .~ []
        -- AND: Prepare expectations
        let expState = BSDefault
        -- WHEN:
        eitherResState <- runInContext (getBranchState branch) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSEdited - edit events" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        -- AND: Prepare branch
        let branch = amsterdamBranchWithEvents
        -- AND: Prepare expectations
        let expState = BSEdited
        -- WHEN:
        eitherResState <- runInContext (getBranchState branch) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSEdited - edit events and new parent package version is available" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (insertPackage netherlandsPackageV2) appContext
        -- AND: Prepare branch
        let branch = amsterdamBranchWithEvents
        -- AND: Prepare expectations
        let expState = BSEdited
        -- WHEN:
        eitherResState <- runInContext (getBranchState branch) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSOutdated - no edit events and new parent package version is available" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (insertPackage netherlandsPackageV2) appContext
        -- AND: Prepare branch
        let branch = amsterdamBranchWithEvents & events .~ []
        -- AND: Prepare expectations
        let expState = BSOutdated
        -- WHEN:
        eitherResState <- runInContext (getBranchState branch) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSMigrating - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (insertPackage netherlandsPackageV2) appContext
        runInContext (deleteEventsAtBranch branchUuid) appContext
        let migratorCreateDto =
              MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
        runInContext (createMigration branchUuid migratorCreateDto) appContext
        -- AND: Prepare branch
        let branch = amsterdamBranchWithEvents
        -- AND: Prepare expectations
        let expState = BSMigrating
        -- WHEN:
        eitherResState <- runInContext (getBranchState branch) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSMigrated - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        let migratorCreateDto =
              MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}
        runInContext (createMigration branchUuid migratorCreateDto) appContext
        let reqDto =
              MigratorConflictDTO
                { _migratorConflictDTOOriginalEventUuid = a_km1_ch4 ^. uuid
                , _migratorConflictDTOAction = MCAReject
                , _migratorConflictDTOEvent = Nothing
                }
        runInContext (solveConflictAndMigrate branchUuid reqDto) appContext
        -- AND: Prepare branch
        let branch = amsterdamBranchWithEvents & events .~ []
        -- AND: Prepare expectations
        let expState = BSMigrated
        -- WHEN:
        eitherResState <- runInContext (getBranchState branch) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
