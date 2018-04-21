module Service.Migrator.Sanitizator where

import Control.Lens hiding (find)
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.UUID as U
import Text.Pretty.Simple (pPrint)

import Common.Utils
import Common.Uuid
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.Migrator.MigratorState

-- ------------------------------------------------------------
class Sanitizator a where
  sanitize :: MigratorState -> a -> IO a

-- ------------------------------------------------------------
instance Sanitizator EditKnowledgeModelEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChapterUuids $ \childIdsFromEvent ->
        changeEventUuid uuid $ event & chapterIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
    where
      unwrapEventChapterUuids callback =
        case event ^. chapterIds of
          NothingChanged -> return event
          ChangedValue uuids -> callback uuids
      childIdsFromKM :: KnowledgeModel -> [U.UUID]
      childIdsFromKM km = _chapterUuid <$> getAllChapters km
      isInChildIds :: KnowledgeModel -> U.UUID -> Bool
      isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childIdsFromEvent =
        filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditChapterEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childIdsFromEvent ->
        changeEventUuid uuid $ event & questionIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
    where
      unwrapEventChildUuids callback =
        case event ^. questionIds of
          NothingChanged -> return event
          ChangedValue uuids -> callback uuids
      childIdsFromKM :: KnowledgeModel -> [U.UUID]
      childIdsFromKM km = _questionUuid <$> getAllQuestionsForChapterUuid km (event ^. chapterUuid)
      isInChildIds :: KnowledgeModel -> U.UUID -> Bool
      isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childIdsFromEvent =
        filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

--        pPrint $ km
--        pPrint $ childIdsFromEvent
--        pPrint $ childIdsFromKM km
--        pPrint $ childIdsFromEvent ++ childIdsFromKM km
--        pPrint $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
--        pPrint $ filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
-- ------------------------------------------------------------
instance Sanitizator EditQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyAnswerChange km event
      event2 <- applyReferenceChange km event1
      event3 <- applyExpertChange km event2
      changeEventUuid uuid event3
      -- ------------------------
      -- Answers
      -- ------------------------
    where
      applyAnswerChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & answerIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. answerIds of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _answerUuid <$> getAllAnswersForQuestionUuid km (event ^. questionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> Maybe [U.UUID] -> Maybe [U.UUID]
          resultUuids km maybeChildIdsFromEvent =
            case maybeChildIdsFromEvent of
              Just childIdsFromEvent ->
                Just $ filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
              Nothing -> Nothing
      -- ------------------------
      -- References
      -- ------------------------
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & referenceIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. referenceIds of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _referenceUuid <$> getAllReferencesForQuestionUuid km (event ^. questionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent =
            filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
      -- ------------------------
      -- Experts
      -- ------------------------
      applyExpertChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & expertIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. expertIds of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _expertUuid <$> getAllExpertsForQuestionUuid km (event ^. questionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent =
            filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditAnswerEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childIdsFromEvent ->
        changeEventUuid uuid $ event & followUpIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
    where
      unwrapEventChildUuids callback =
        case event ^. followUpIds of
          NothingChanged -> return event
          ChangedValue uuids -> callback uuids
      childIdsFromKM :: KnowledgeModel -> [U.UUID]
      childIdsFromKM km = _questionUuid <$> getAllQuestionsForAnswerUuid km (event ^. answerUuid)
      isInChildIds :: KnowledgeModel -> U.UUID -> Bool
      isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childIdsFromEvent =
        filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditFollowUpQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyAnswerChange km event
      event2 <- applyReferenceChange km event1
      event3 <- applyExpertChange km event2
      changeEventUuid uuid event3
      -- ------------------------
      -- Answers
      -- ------------------------
    where
      applyAnswerChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & answerIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. answerIds of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _answerUuid <$> getAllAnswersForQuestionUuid km (event ^. questionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> Maybe [U.UUID] -> Maybe [U.UUID]
          resultUuids km maybeChildIdsFromEvent =
            case maybeChildIdsFromEvent of
              Just childIdsFromEvent ->
                Just $ filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
              Nothing -> Nothing
      -- ------------------------
      -- References
      -- ------------------------
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & referenceIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. referenceIds of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _referenceUuid <$> getAllReferencesForQuestionUuid km (event ^. questionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent =
            filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
      -- ------------------------
      -- Experts
      -- ------------------------
      applyExpertChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & expertIds .~ (ChangedValue $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. expertIds of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _expertUuid <$> getAllExpertsForQuestionUuid km (event ^. questionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (== uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent =
            filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------
-- ------------------------------------------------------------
unwrapKM state event callback =
  case state ^. msCurrentKnowledgeModel of
    Nothing -> return event
    Just km -> callback km

changeEventUuid setter event = do
  uuid <- generateUuid
  return $ event & setter .~ uuid
