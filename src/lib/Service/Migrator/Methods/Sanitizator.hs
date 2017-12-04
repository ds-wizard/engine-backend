module Service.Migrator.Methods.Sanitizator where

import Control.Lens hiding(find)
import qualified Data.Set as S
import qualified Data.UUID as U
import Data.Maybe
import Data.List
import Text.Pretty.Simple (pPrint)

import Common.Uuid
import Common.Utils
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Migrator.MigratorState
import Model.KnowledgeModel.KnowledgeModel

-- ------------------------------------------------------------

class Sanitizator a where
  sanitize :: MigratorState -> a -> IO a

-- ------------------------------------------------------------

instance Sanitizator EditKnowledgeModelEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChapterUuids $ \childIdsFromEvent ->
        changeEventUuid ekmUuid $ event & ekmChapterIds .~ (Just $ resultUuids km childIdsFromEvent)
    where
      unwrapEventChapterUuids callback =
        case event ^. ekmChapterIds of
          Nothing -> return event
          Just uuids -> callback uuids
      childIdsFromKM :: KnowledgeModel -> [U.UUID]
      childIdsFromKM km = _chUuid <$> getAllChapters km
      isInChildIds :: KnowledgeModel -> U.UUID -> Bool
      isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------

instance Sanitizator EditChapterEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childIdsFromEvent ->
--        pPrint $ km
--        pPrint $ childIdsFromEvent
--        pPrint $ childIdsFromKM km
--        pPrint $ childIdsFromEvent ++ childIdsFromKM km
--        pPrint $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
--        pPrint $ filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
        changeEventUuid echUuid $ event & echQuestionIds .~ (Just $ resultUuids km childIdsFromEvent)
    where
      unwrapEventChildUuids callback =
        case event ^. echQuestionIds of
          Nothing -> return event
          Just uuids -> callback uuids
      childIdsFromKM :: KnowledgeModel -> [U.UUID]
      childIdsFromKM km = _qUuid <$> getAllQuestionsForChapterUuid km (event ^. echChapterUuid)
      isInChildIds :: KnowledgeModel -> U.UUID -> Bool
      isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------

instance Sanitizator EditQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyAnswerChange km event
      event2 <- applyReferenceChange km event1
      event3 <- applyExpertChange km event2
      changeEventUuid eqUuid event3
    where
      -- ------------------------
      -- Answers
      -- ------------------------
      applyAnswerChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & eqAnswerIds .~ (Just $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. eqAnswerIds of
              Nothing -> return event
              Just uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _ansUuid <$> getAllAnswersForQuestionUuid km (event ^. eqQuestionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
      -- ------------------------
      -- References
      -- ------------------------
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & eqReferenceIds .~ (Just $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. eqReferenceIds of
              Nothing -> return event
              Just uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _refUuid <$> getAllReferencesForQuestionUuid km (event ^. eqQuestionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
      -- ------------------------
      -- Experts
      -- ------------------------
      applyExpertChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & eqExpertIds .~ (Just $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. eqExpertIds of
              Nothing -> return event
              Just uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _expUuid <$> getAllExpertsForQuestionUuid km (event ^. eqQuestionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------

instance Sanitizator EditAnswerEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childIdsFromEvent ->
        changeEventUuid eansUuid $ event & eansFollowUpIds .~ (Just $ resultUuids km childIdsFromEvent)
    where
      unwrapEventChildUuids callback =
        case event ^. eansFollowUpIds of
          Nothing -> return event
          Just uuids -> callback uuids
      childIdsFromKM :: KnowledgeModel -> [U.UUID]
      childIdsFromKM km = _qUuid <$> getAllQuestionsForAnswerUuid km (event ^. eansAnswerUuid)
      isInChildIds :: KnowledgeModel -> U.UUID -> Bool
      isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------

instance Sanitizator EditFollowUpQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyAnswerChange km event
      event2 <- applyReferenceChange km event1
      event3 <- applyExpertChange km event2
      changeEventUuid efuqUuid event3
    where
      -- ------------------------
      -- Answers
      -- ------------------------
      applyAnswerChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & efuqAnswerIds .~ (Just $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. efuqAnswerIds of
              Nothing -> return event
              Just uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _ansUuid <$> getAllAnswersForQuestionUuid km (event ^. efuqQuestionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
      -- ------------------------
      -- References
      -- ------------------------
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & efuqReferenceIds .~ (Just $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. efuqReferenceIds of
              Nothing -> return event
              Just uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _refUuid <$> getAllReferencesForQuestionUuid km (event ^. efuqQuestionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km
      -- ------------------------
      -- Experts
      -- ------------------------
      applyExpertChange km event =
        unwrapEventChildUuids $ \childIdsFromEvent ->
          return $ event & efuqExpertIds .~ (Just $ resultUuids km childIdsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. efuqExpertIds of
              Nothing -> return event
              Just uuids -> callback uuids
          childIdsFromKM :: KnowledgeModel -> [U.UUID]
          childIdsFromKM km = _expUuid <$> getAllExpertsForQuestionUuid km (event ^. efuqQuestionUuid)
          isInChildIds :: KnowledgeModel -> U.UUID -> Bool
          isInChildIds km uuid = isJust $ find (==uuid) (childIdsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childIdsFromEvent = filter (isInChildIds km) $ removeDuplicates $ childIdsFromEvent ++ childIdsFromKM km

-- ------------------------------------------------------------
-- ------------------------------------------------------------

unwrapKM state event callback =
  case state ^. msCurrentKnowledgeModel of
    Nothing -> return event
    Just km -> callback km

changeEventUuid setter event = do
  uuid <- generateUuid
  return $ event & setter .~ uuid

