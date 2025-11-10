module Wizard.Service.KnowledgeModel.Migration.Migrator.CorrectorMethod (
  runCorrectorMethod,
) where

import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Migration.Migrator.Sanitizer

runCorrectorMethod :: KnowledgeModelMigration -> KnowledgeModelEvent -> AppContextM KnowledgeModelMigration
runCorrectorMethod state event = do
  logInfoI _CMP_SERVICE . f' "Running corrector method for event '%s'" $ [U.toString event.uuid]
  sanitizedEvent <- sanitizeEvent state event
  let sanitizedTargetPackageEvents = sanitizeTargetPackageEvents state.targetPackageEvents sanitizedEvent
  return
    state
      { state = ConflictKnowledgeModelMigrationState {targetEvent = Just sanitizedEvent}
      , targetPackageEvents = sanitizedTargetPackageEvents
      }

sanitizeTargetPackageEvents :: [KnowledgeModelEvent] -> KnowledgeModelEvent -> [KnowledgeModelEvent]
sanitizeTargetPackageEvents [] _ = []
sanitizeTargetPackageEvents (firstEvent : restEvent) sanitizedFirstEvent = sanitizedFirstEvent : restEvent
