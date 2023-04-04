module Wizard.Specs.Service.Branch.BranchServiceSpec where

import Control.Monad.Reader
import Data.Either
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Database.DAO.Branch.BranchDataDAO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Branch.Data.Branches
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchState
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Branch.BranchUtil
import Wizard.Service.Migration.KnowledgeModel.MigratorService

import Wizard.Specs.Common

branchServiceIntegrationSpec appContext =
  describe "Branch Service Integration" $ do
    describe "getBranchState" $ do
      it "BSDefault - no edit events, no new parent package version" $
        -- GIVEN: Prepare database
        do
          runInContext PKG.runMigration appContext
          runInContext B.runMigration appContext
          runInContext (deletePackageById netherlandsPackageV2.pId) appContext
          -- AND: Prepare branch
          let branch = amsterdamBranch
          let branchData = amsterdamBranchData {events = []} :: BranchData
          let forkOfPackageId = Just netherlandsPackage.pId
          -- AND: Prepare expectations
          let expState = BSDefault
          -- WHEN:
          eitherResState <-
            runInContext (getBranchState branch (length branchData.events) forkOfPackageId) appContext
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
          let branch = amsterdamBranch
          let branchData = amsterdamBranchData
          let forkOfPackageId = Just netherlandsPackage.pId
          -- AND: Prepare expectations
          let expState = BSEdited
          -- WHEN:
          eitherResState <-
            runInContext (getBranchState branch (length branchData.events) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "BSEdited - edit events and new parent package version is available" $
        -- GIVEN: Prepare database
        do
          runInContext PKG.runMigration appContext
          runInContext B.runMigration appContext
          -- AND: Prepare branch
          let branch = amsterdamBranch
          let branchData = amsterdamBranchData
          let forkOfPackageId = Just netherlandsPackage.pId
          -- AND: Prepare expectations
          let expState = BSEdited
          -- WHEN:
          eitherResState <-
            runInContext (getBranchState branch (length branchData.events) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "BSOutdated - no edit events and new parent package version is available" $
        -- GIVEN: Prepare database
        do
          runInContext PKG.runMigration appContext
          runInContext B.runMigration appContext
          -- AND: Prepare branch
          let branch = amsterdamBranch
          let branchData = amsterdamBranchData {events = []} :: BranchData
          let forkOfPackageId = Just netherlandsPackage.pId
          -- AND: Prepare expectations
          let expState = BSOutdated
          -- WHEN:
          eitherResState <-
            runInContext (getBranchState branch (length branchData.events) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "BSMigrating - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
        do
          runInContext PKG.runMigration appContext
          runInContext B.runMigration appContext
          runInContext (updateBranchEventsByUuid amsterdamBranch.uuid []) appContext
          let migratorCreateDto =
                MigratorStateCreateDTO {targetPackageId = netherlandsPackageV2.pId}
          runInContext (createMigration amsterdamBranch.uuid migratorCreateDto) appContext
          -- AND: Prepare branch
          let branch = amsterdamBranch
          let branchData = amsterdamBranchData
          let forkOfPackageId = Just netherlandsPackage.pId
          -- AND: Prepare expectations
          let expState = BSMigrating
          -- WHEN:
          eitherResState <-
            runInContext (getBranchState branch (length branchData.events) forkOfPackageId) appContext
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
                MigratorStateCreateDTO {targetPackageId = netherlandsPackageV2.pId}
          runInContext (createMigration amsterdamBranch.uuid migratorCreateDto) appContext
          let reqDto =
                MigratorConflictDTO
                  { originalEventUuid = a_km1_ch4.uuid
                  , action = MCAReject
                  , event = Nothing
                  }
          runInContext (solveConflictAndMigrate amsterdamBranch.uuid reqDto) appContext
          -- AND: Prepare branch
          let branch = amsterdamBranch
          let branchData = amsterdamBranchData {events = []} :: BranchData
          let forkOfPackageId = Just netherlandsPackage.pId
          -- AND: Prepare expectations
          let expState = BSMigrated
          -- WHEN:
          eitherResState <-
            runInContext (getBranchState branch (length branchData.events) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
