module KMMigration.Model.Event where

import Control.Lens

import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel
-- data EventGroup = EventGroup
--   { _kmeUuid :: UUID
--   -- , _kmeNamespace :: String
--   , _kmeCreatedAt :: DateTime
--   , _kmeName :: String
--   , _kmeReason :: String
--   -- , _kmeEvents :: [Event]
--   }
-- class Event ch where
--   eType :: ch -> String
-- data AddChapterEvent = AddChapterEvent
--   { _kmeAcheChapter :: Chapter
--   , _kmeAcheQuestion :: Question
--   }
-- instance Event AddChapterEvent where
--   eType _ = "ADD_CHAPTER_EVENT"
--   ### Chapter
--   - ADD_CHAPTER
--   - MODIFY_CHAPTER
--   - DELETE_CHAPTER
--   ### Question
--   - ADD_QUESTION
--   - MODIFY_QUESTION
--   - DELETE_QUESTION
--   ### Answer
--   * ADD_ANSWER
--   * MODIFY_ANSWER
--   * DELETE_ANSWER
--   ### Expert
--   * ADD_EXPERT
--   * MODIFY_EXPERT
--   * DELETE_EXPERT
--   ### Reference
--   * ADD_REFERENCE
--   * MODIFY_REFERENCE
--   * DELETE_REFERENCE
