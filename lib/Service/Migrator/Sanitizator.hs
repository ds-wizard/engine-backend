module Service.Migrator.Sanitizator where

import Control.Lens hiding (find)
import Data.List
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.Migrator.MigratorState
import Util.List
import Util.Uuid

-- ------------------------------------------------------------
class Sanitizator a where
  sanitize :: MigratorState -> a -> IO a

-- ------------------------------------------------------------
instance Sanitizator EditKnowledgeModelEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChapterUuids $ \childUuidsFromEvent ->
        changeEventUuid uuid $ event & chapterUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
    where
      unwrapEventChapterUuids callback =
        case event ^. chapterUuids of
          NothingChanged -> return event
          ChangedValue uuids -> callback uuids
      childUuidsFromKM :: KnowledgeModel -> [U.UUID]
      childUuidsFromKM km = _chapterUuid <$> getAllChapters km
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
      childUuidsFromKM km = _questionUuid <$> getAllQuestionsForChapterUuid km (event ^. chapterUuid)
      isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
      isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
      resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
      resultUuids km childUuidsFromEvent =
        filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyAnswerItemTemplateChange km event
      event2 <- applyAnswerChange km event1
      event3 <- applyReferenceChange km event2
      event4 <- applyExpertChange km event3
      changeEventUuid uuid event4
      -- ------------------------
      -- Answer Item Template
      -- ------------------------
    where
      applyAnswerItemTemplateChange km event =
        unwrapEventAitPlainWithUuids $ \aitPlainWithUuids ->
          return $
          event & answerItemTemplatePlainWithUuids .~ (ChangedValue . Just $ newAitPlainWithUuids aitPlainWithUuids)
        where
          unwrapEventAitPlainWithUuids callback =
            case event ^. answerItemTemplatePlainWithUuids of
              NothingChanged -> return event
              ChangedValue maybeAitPlainWithUuids ->
                case maybeAitPlainWithUuids of
                  Just aitPlainWithUuids -> callback aitPlainWithUuids
                  Nothing -> return event
          newAitPlainWithUuids :: AnswerItemTemplatePlainWithUuids -> AnswerItemTemplatePlainWithUuids
          newAitPlainWithUuids aitPlainWithUuids =
            aitPlainWithUuids & questionUuids .~ resultUuids km (aitPlainWithUuids ^. questionUuids)
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = _questionUuid <$> getAllAitQuestionsForParentQuestionUuid km (event ^. questionUuid)
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
      -- ------------------------
      -- Answers
      -- ------------------------
      applyAnswerChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & answerUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. answerUuids of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = _answerUuid <$> getAllAnswersForQuestionUuid km (event ^. questionUuid)
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> Maybe [U.UUID] -> Maybe [U.UUID]
          resultUuids km maybeChildUuidsFromEvent =
            case maybeChildUuidsFromEvent of
              Just childUuidsFromEvent ->
                Just $ filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
              Nothing -> Nothing
      -- ------------------------
      -- References
      -- ------------------------
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return $ event & referenceUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. referenceUuids of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getReferenceUuid <$> getAllReferencesForQuestionUuid km (event ^. questionUuid)
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
          return $ event & expertUuids .~ (ChangedValue $ resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case event ^. expertUuids of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = _expertUuid <$> getAllExpertsForQuestionUuid km (event ^. questionUuid)
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
      childUuidsFromKM km = _questionUuid <$> getAllQuestionsForAnswerUuid km (event ^. answerUuid)
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
