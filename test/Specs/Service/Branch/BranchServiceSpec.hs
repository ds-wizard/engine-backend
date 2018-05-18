module Specs.Service.Branch.BranchServiceSpec where

import Control.Lens
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Api.Resource.Migrator.MigratorConflictDTO
import Api.Resource.Migrator.MigratorStateCreateDTO
import Database.DAO.Event.EventDAO
import Database.DAO.Package.PackageDAO
import qualified Database.Migration.Branch.BranchMigration as B
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Package.Data.Package
import qualified Database.Migration.Package.PackageMigration as PKG
import LensesConfig
import Model.Branch.BranchState
import Model.Migrator.MigratorState
import Service.Branch.BranchService
import Service.Migrator.MigratorService

import Specs.Common

branchServiceSpec =
  describe "Package Service" $
  it "isVersionInValidFormat" $ do
    isNothing (isValidKmId "core") `shouldBe` True
    isNothing (isValidKmId "ab") `shouldBe` True
    isNothing (isValidKmId "core-nl") `shouldBe` True
    isNothing (isValidKmId "core-nl-amsterdam") `shouldBe` True
    isJust (isValidKmId "a") `shouldBe` True
    isJust (isValidKmId "core.nl") `shouldBe` True
    isJust (isValidKmId "a.b") `shouldBe` True
    isJust (isValidKmId "core_nl") `shouldBe` True

branchServiceIntegrationSpec appContext =
  describe "Branch Service Integration" $ do
    let branchUuid = "6474b24b-262b-42b1-9451-008e8363f2b6"
    describe "getBranchState" $ do
      it "BSDefault - no edit events, no new parent package version" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (deleteEventsAtBranch branchUuid) appContext
        runInContext (deletePackageById (elixirNlPackage2Dto ^. pId)) appContext
        -- AND: Prepare expectations
        let expState = BSDefault
        -- WHEN:
        eitherResState <- runInContext (getBranchState branchUuid) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSEdited - edit events" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        -- AND: Prepare expectations
        let expState = BSEdited
        -- WHEN:
        eitherResState <- runInContext (getBranchState branchUuid) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSEdited - edit events and new parent package version is available" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (insertPackage elixirNlPackage2Dto) appContext
        -- AND: Prepare expectations
        let expState = BSEdited
        -- WHEN:
        eitherResState <- runInContext (getBranchState branchUuid) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSOutdated - no edit events and new parent package version is available" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (insertPackage elixirNlPackage2Dto) appContext
        runInContext (deleteEventsAtBranch branchUuid) appContext
        -- AND: Prepare expectations
        let expState = BSOutdated
        -- WHEN:
        eitherResState <- runInContext (getBranchState branchUuid) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSMigrating - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (insertPackage elixirNlPackage2Dto) appContext
        runInContext (deleteEventsAtBranch branchUuid) appContext
        let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pId}
        runInContext (createMigration branchUuid migratorCreateDto) appContext
        -- AND: Prepare expectations
        let expState = BSMigrating
        -- WHEN:
        eitherResState <- runInContext (getBranchState branchUuid) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSMigrated - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
       do
        runInContext PKG.runMigration appContext
        runInContext B.runMigration appContext
        runInContext (deleteEventsAtBranch branchUuid) appContext
        let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pId}
        runInContext (createMigration branchUuid migratorCreateDto) appContext
        let reqDto =
              MigratorConflictDTO
              {_mcdtoOriginalEventUuid = a_km1_ch3 ^. uuid, _mcdtoAction = MCAReject, _mcdtoEvent = Nothing}
        runInContext (solveConflictAndMigrate branchUuid reqDto) appContext
        -- AND: Prepare expectations
        let expState = BSMigrated
        -- WHEN:
        eitherResState <- runInContext (getBranchState branchUuid) appContext
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
