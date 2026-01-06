module Wizard.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelEditorMigrator (
  migrateAll,
) where

import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import qualified Data.Vector as Vector

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelRawEventJM ()
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Service.KnowledgeModel.Editor.EditorMapper
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.CommonDB

migrateAll :: AppContextM ()
migrateAll = do
  logMigrationStarted "knowledge_model_Editor"
  kmPkgs <- findEditorsByUnsupportedMetamodelVersion knowledgeModelMetamodelVersion
  tenantUuid <- asks currentTenantUuid
  traverse_ (migrateOneInDB tenantUuid) kmPkgs
  logMigrationCompleted "knowledge_model_Editor"

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: U.UUID -> KnowledgeModelEditor -> AppContextM ()
migrateOneInDB tenantUuid editor = do
  kmEvents <- findKnowledgeModelRawEventsByEditorUuid editor.uuid
  let events = A.Array . Vector.fromList . fmap (A.toJSON . toKnowledgeModelRawEvent) $ kmEvents
  migrateEventField "knowledge_model_editor" editor.createdAt editor.metamodelVersion events $ \eventsMigratedValue -> do
    case A.fromJSON eventsMigratedValue of
      A.Error error -> logMigrationFailedToConvertToNewMetamodelVersion "knowledge_model_editor" error
      A.Success eventsMigrated -> do
        deleteKnowledgeModelEventsByEditorUuid editor.uuid
        let kmEventsMigrated = fmap (toKnowledgeModelEditorRawEvent editor.uuid tenantUuid) eventsMigrated
        traverse_ insertKnowledgeModelRawEvent kmEventsMigrated
