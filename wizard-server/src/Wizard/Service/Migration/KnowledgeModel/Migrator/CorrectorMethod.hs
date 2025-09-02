module Wizard.Service.Migration.KnowledgeModel.Migrator.CorrectorMethod (
  runCorrectorMethod,
) where

import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Logger
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Migration.KnowledgeModel.Migrator.Sanitizer
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventLenses ()

runCorrectorMethod :: MigratorState -> Event -> AppContextM MigratorState
runCorrectorMethod state event = do
  logInfoI _CMP_SERVICE . f' "Running corrector method for event '%s'" $ [U.toString . getUuid $ event]
  sanitizedEvent <- sanitizeEvent state event
  let sanitizedTargetPackageEvents = sanitizeTargetPackageEvents state.targetPackageEvents sanitizedEvent
  return
    state
      { migrationState = ConflictState . CorrectorConflict . Just $ sanitizedEvent
      , targetPackageEvents = sanitizedTargetPackageEvents
      }

sanitizeEvent :: MigratorState -> Event -> AppContextM Event
sanitizeEvent state (EditKnowledgeModelEvent' e) = sanitize state e >>= \e2 -> return . EditKnowledgeModelEvent' $ e2
sanitizeEvent state (EditChapterEvent' e) = sanitize state e >>= \e2 -> return . EditChapterEvent' $ e2
sanitizeEvent state (EditQuestionEvent' e) = sanitize state e >>= \e2 -> return . EditQuestionEvent' $ e2
sanitizeEvent state (EditAnswerEvent' e) = sanitize state e >>= \e2 -> return . EditAnswerEvent' $ e2
sanitizeEvent state event = return event

sanitizeTargetPackageEvents :: [Event] -> Event -> [Event]
sanitizeTargetPackageEvents [] _ = []
sanitizeTargetPackageEvents (firstEvent : restEvent) sanitizedFirstEvent = sanitizedFirstEvent : restEvent
