module Wizard.Specs.Service.KnowledgeModel.Editor.KnowledgeModelEditorServiceSpec where

import Control.Monad.Reader
import Data.Either
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Editor.EditorUtil
import Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

import Wizard.Specs.Common

knowledgeModelEditorServiceSpec appContext =
  describe "Knowledge Model Editor Service Integration" $ do
    describe "getEditorState" $ do
      it "DefaultKnowledgeModelEditorState - no edit events, no new parent package version" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration appContext
          runInContext KnowledgeModelEditor.runMigration appContext
          runInContext (deletePackageById netherlandsKmPackageV2.pId) appContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = []
          let forkOfPackageId = Just netherlandsKmPackage.pId
          -- AND: Prepare expectations
          let expState = DefaultKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "EditedKnowledgeModelEditorState - edit events" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration appContext
          runInContext KnowledgeModelEditor.runMigration appContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = amsterdamKnowledgeModelEditorEvents
          let forkOfPackageId = Just netherlandsKmPackage.pId
          -- AND: Prepare expectations
          let expState = EditedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "EditedKnowledgeModelEditorState - edit events and new parent package version is available" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration appContext
          runInContext KnowledgeModelEditor.runMigration appContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = amsterdamKnowledgeModelEditorEvents
          let forkOfPackageId = Just netherlandsKmPackage.pId
          -- AND: Prepare expectations
          let expState = EditedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "OutdatedKnowledgeModelEditorState - no edit events and new parent package version is available" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration appContext
          runInContext KnowledgeModelEditor.runMigration appContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = []
          let forkOfPackageId = Just netherlandsKmPackage.pId
          -- AND: Prepare expectations
          let expState = OutdatedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "MigratingKnowledgeModelEditorState - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration appContext
          runInContext KnowledgeModelEditor.runMigration appContext
          runInContext (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditor.uuid) appContext
          let migratorCreateDto =
                KnowledgeModelMigrationCreateDTO {targetPackageId = netherlandsKmPackageV2.pId}
          runInContext (createMigration amsterdamKnowledgeModelEditor.uuid migratorCreateDto) appContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = amsterdamKnowledgeModelEditorEvents
          let forkOfPackageId = Just netherlandsKmPackage.pId
          -- AND: Prepare expectations
          let expState = MigratingKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "MigratedKnowledgeModelEditorState - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration appContext
          runInContext KnowledgeModelEditor.runMigration appContext
          let migratorCreateDto =
                KnowledgeModelMigrationCreateDTO {targetPackageId = netherlandsKmPackageV2.pId}
          runInContext (createMigration amsterdamKnowledgeModelEditor.uuid migratorCreateDto) appContext
          let reqDto =
                KnowledgeModelMigrationResolutionDTO
                  { originalEventUuid = a_km1_ch4.uuid
                  , action = RejectKnowledgeModelMigrationAction
                  }
          runInContext (solveConflictAndMigrate amsterdamKnowledgeModelEditor.uuid reqDto) appContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = []
          let forkOfPackageId = Just netherlandsKmPackage.pId
          -- AND: Prepare expectations
          let expState = MigratedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) appContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
