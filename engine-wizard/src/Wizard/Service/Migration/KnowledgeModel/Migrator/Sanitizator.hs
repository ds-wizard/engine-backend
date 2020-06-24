module Wizard.Service.Migration.KnowledgeModel.Migrator.Sanitizator where

import Control.Lens hiding (find)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.EventLenses
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Uuid
import Wizard.Model.Event.EventLenses
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Util.List

-- ------------------------------------------------------------
class Sanitizator a where
  sanitize :: MigratorState -> a -> IO a

-- ------------------------------------------------------------
instance Sanitizator EditKnowledgeModelEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyChapterChange km event
      event2 <- applyTagChange km event1
      event3 <- applyIntegrationChange km event2
      changeEventUuid uuid event3
      -- ------------------------
      -- Chapter
      -- ------------------------
    where
      applyChapterChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & chapterUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. chapterUuids of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = M.keys $ km ^. chaptersM
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
      -- ------------------------
      -- Tag
      -- ------------------------
      applyTagChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & tagUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. tagUuids of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = M.keys $ km ^. tagsM
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
      -- ------------------------
      -- Integration
      -- ------------------------
      applyIntegrationChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & integrationUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. integrationUuids of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = M.keys $ km ^. integrationsM
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditChapterEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childUuidsFromEvent ->
        changeEventUuid uuid $ event & questionUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
    where
      unwrapEventChildUuids callback =
        case event ^. questionUuids of
          NothingChanged -> return event
          ChangedValue uuids -> callback uuids
      childUuidsFromKM :: KnowledgeModel -> [U.UUID]
      childUuidsFromKM km = getQuestionUuidsForChapterUuid km (event ^. entityUuid)
      isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
      isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childUuidsFromEvent =
        filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyExpertChange km event
      event2 <- applyReferenceChange km event1
      event3 <- applyAnswerChange km event2
      event4 <- applyItemTemplateQuestionChange km event3
      changeEventUuid eUuid' event4
      -- ------------------------
      -- Answer Item Template
      -- ------------------------
    where
      applyItemTemplateQuestionChange km (EditListQuestionEvent' event) =
        unwrapEventItemTemplateQuestionUuids $ \itqUuids ->
          return . EditListQuestionEvent' $
          event & itemTemplateQuestionUuids .~ (ChangedValue $ resultUuids km itqUuids)
        where
          unwrapEventItemTemplateQuestionUuids callback =
            case event ^. itemTemplateQuestionUuids of
              NothingChanged -> return . EditListQuestionEvent' $ event
              ChangedValue itqUuids -> callback itqUuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getItemTemplateQuestionUuidsForQuestionUuid km (event ^. entityUuid)
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
      applyItemTemplateQuestionChange km event = return event
      -- ------------------------
      -- Answers
      -- ------------------------
      applyAnswerChange km (EditOptionsQuestionEvent' event) =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return . EditOptionsQuestionEvent' $
          event & answerUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. answerUuids of
              NothingChanged -> return . EditOptionsQuestionEvent' $ event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getAnswerUuidsForQuestionUuid km (event ^. entityUuid)
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
      applyAnswerChange km event = return event
      -- ------------------------
      -- References
      -- ------------------------
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & eReferenceUuids' .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. eReferenceUuids' of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getReferenceUuidsForQuestionUuid km (event ^. entityUuid')
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
      -- ------------------------
      -- Experts
      -- ------------------------
      applyExpertChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & eExpertUuids' .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. eExpertUuids' of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getExpertUuidsForQuestionUuid km (event ^. entityUuid')
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditAnswerEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childUuidsFromEvent ->
        changeEventUuid uuid $ event & followUpUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
    where
      unwrapEventChildUuids callback =
        case event ^. followUpUuids of
          NothingChanged -> return event
          ChangedValue uuids -> callback uuids
      childUuidsFromKM :: KnowledgeModel -> [U.UUID]
      childUuidsFromKM km = getQuestionUuidsForAnswerUuid km (event ^. entityUuid)
      isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
      isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childUuidsFromEvent =
        filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
-- ------------------------------------------------------------
unwrapKM state event callback =
  case state ^. currentKnowledgeModel of
    Nothing -> return event
    Just km -> callback km

changeEventUuid setter event = do
  uuid <- generateUuid
  return $ event & setter .~ uuid
