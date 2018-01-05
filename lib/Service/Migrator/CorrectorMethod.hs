module Service.Migrator.CorrectorMethod
  ( runCorrectorMethod
  ) where

import Control.Lens

import Model.Event.Event
import Model.Migrator.MigratorState
import Service.Migrator.Sanitizator

runCorrectorMethod :: MigratorState -> Event -> IO MigratorState
runCorrectorMethod state event = do
  sanitizedEvent <- sanitizeEvent state event
  return $ state & msMigrationState .~ (ConflictState . CorrectorConflict $ sanitizedEvent)

sanitizeEvent :: MigratorState -> Event -> IO Event
sanitizeEvent state (EditKnowledgeModelEvent' e) = sanitize state e >>= \e2 -> return . EditKnowledgeModelEvent' $ e2
sanitizeEvent state (EditChapterEvent' e) = sanitize state e >>= \e2 -> return . EditChapterEvent' $ e2
sanitizeEvent state (EditQuestionEvent' e) = sanitize state e >>= \e2 -> return . EditQuestionEvent' $ e2
sanitizeEvent state (EditAnswerEvent' e) = sanitize state e >>= \e2 -> return . EditAnswerEvent' $ e2
sanitizeEvent state (EditFollowUpQuestionEvent' e) =
  sanitize state e >>= \e2 -> return . EditFollowUpQuestionEvent' $ e2
sanitizeEvent state event = return event
