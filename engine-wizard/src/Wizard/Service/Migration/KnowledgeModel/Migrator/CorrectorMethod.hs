module Wizard.Service.Migration.KnowledgeModel.Migrator.CorrectorMethod
  ( runCorrectorMethod
  ) where

import Control.Lens

import LensesConfig
import Shared.Model.Event.Event
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Migration.KnowledgeModel.Migrator.Sanitizator

runCorrectorMethod :: MigratorState -> Event -> IO MigratorState
runCorrectorMethod state event = do
  sanitizedEvent <- sanitizeEvent state event
  let sanitizedTargetPackageEvents = sanitizeTargetPackageEvents (state ^. targetPackageEvents) sanitizedEvent
  return .
    (migrationState .~ (ConflictState . CorrectorConflict . Just $ sanitizedEvent)) .
    (targetPackageEvents .~ sanitizedTargetPackageEvents) $
    state

sanitizeEvent :: MigratorState -> Event -> IO Event
sanitizeEvent state (EditKnowledgeModelEvent' e) = sanitize state e >>= \e2 -> return . EditKnowledgeModelEvent' $ e2
sanitizeEvent state (EditChapterEvent' e) = sanitize state e >>= \e2 -> return . EditChapterEvent' $ e2
sanitizeEvent state (EditQuestionEvent' e) = sanitize state e >>= \e2 -> return . EditQuestionEvent' $ e2
sanitizeEvent state (EditAnswerEvent' e) = sanitize state e >>= \e2 -> return . EditAnswerEvent' $ e2
sanitizeEvent state event = return event

sanitizeTargetPackageEvents :: [Event] -> Event -> [Event]
sanitizeTargetPackageEvents [] _ = []
sanitizeTargetPackageEvents (firstEvent:restEvent) sanitizedFirstEvent = sanitizedFirstEvent : restEvent
