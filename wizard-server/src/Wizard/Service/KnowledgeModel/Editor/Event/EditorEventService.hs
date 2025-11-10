module Wizard.Service.KnowledgeModel.Editor.Event.EditorEventService where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Editor.EditorMapper
import Wizard.Service.KnowledgeModel.Squash.Squasher

squashEvents :: AppContextM ()
squashEvents = do
  editorUuids <- findEditorsForSquashing
  traverse_ squashEventsForEditor editorUuids

squashEventsForEditor :: U.UUID -> AppContextM ()
squashEventsForEditor editorUuid =
  runInTransaction $ do
    logInfoI _CMP_SERVICE (f' "Squashing events for KM editor (editorUuid: '%s')" [U.toString editorUuid])
    tenantUuid <- asks currentTenantUuid
    _ <- findKnowledgeModelEditorByUuidForSquashingLocked editorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid editorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    let squashedEvents = squash kmEvents
    let squashedKmEditorEvents = fmap (toKnowledgeModelEditorEvent editorUuid tenantUuid) squashedEvents
    deleteKnowledgeModelEventsByEditorUuid editorUuid
    traverse_ insertKnowledgeModelEvent squashedKmEditorEvents
    logInfoI
      _CMP_SERVICE
      ( f'
          "Squashing for KM editor '%s' finished successfully (before: %s, after %s)"
          [U.toString editorUuid, show . length $ kmEvents, show . length $ squashedEvents]
      )
