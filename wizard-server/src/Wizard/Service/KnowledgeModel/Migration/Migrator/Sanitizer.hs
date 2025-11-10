module Wizard.Service.KnowledgeModel.Migration.Migrator.Sanitizer where

import Control.Monad.Reader (liftIO)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.UUID as U

import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEventLenses ()
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

sanitizeEvent :: KnowledgeModelMigration -> KnowledgeModelEvent -> AppContextM KnowledgeModelEvent
sanitizeEvent state event =
  case event.content of
    EditKnowledgeModelEvent' content -> sanitizeEditKnowledgeModelEvent state event content
    EditChapterEvent' content -> sanitizeEditChapterEvent state event content
    EditQuestionEvent' content -> sanitizeEditQuestionEvent state event content
    EditAnswerEvent' content -> sanitizeEditAnswerEvent state event content
    _ -> return event

-- ------------------------------------------------------------
sanitizeEditKnowledgeModelEvent :: KnowledgeModelMigration -> KnowledgeModelEvent -> EditKnowledgeModelEvent -> AppContextM KnowledgeModelEvent
sanitizeEditKnowledgeModelEvent state event content =
  unwrapKM state event $ \km -> do
    content1 <- applyChapterChange km content
    content2 <- applyTagChange km content1
    content3 <- applyIntegrationChange km content2
    if content /= content3
      then do
        newUuid <- liftIO generateUuid
        return (event {uuid = newUuid, content = EditKnowledgeModelEvent' content3} :: KnowledgeModelEvent)
      else return event
  where
    -- ------------------------
    -- Chapter
    -- ------------------------
    applyChapterChange km event =
      unwrapEventChildUuids $ \childUuidsFromEvent -> do
        let result = content {chapterUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditKnowledgeModelEvent
        return result
      where
        unwrapEventChildUuids callback =
          case content.chapterUuids of
            NothingChanged -> return event
            ChangedValue uuids -> callback uuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = M.keys $ getChaptersM km
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
    -- ------------------------
    -- Tag
    -- ------------------------
    applyTagChange km event =
      unwrapEventChildUuids $ \childUuidsFromEvent -> do
        let result = event {tagUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditKnowledgeModelEvent
        return result
      where
        unwrapEventChildUuids callback =
          case event.tagUuids of
            NothingChanged -> return event
            ChangedValue uuids -> callback uuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = M.keys $ getTagsM km
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
    -- ------------------------
    -- Integration
    -- ------------------------
    applyIntegrationChange km event =
      unwrapEventChildUuids $ \childUuidsFromEvent -> do
        let result = event {integrationUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditKnowledgeModelEvent
        return result
      where
        unwrapEventChildUuids callback =
          case event.integrationUuids of
            NothingChanged -> return event
            ChangedValue uuids -> callback uuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = M.keys $ getIntegrationsM km
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
sanitizeEditChapterEvent :: KnowledgeModelMigration -> KnowledgeModelEvent -> EditChapterEvent -> AppContextM KnowledgeModelEvent
sanitizeEditChapterEvent state event content =
  unwrapKM state event $ \km ->
    unwrapEventChildUuids $ \childUuidsFromEvent -> do
      let content1 = content {questionUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditChapterEvent
      if content /= content1
        then do
          newUuid <- liftIO generateUuid
          return (event {uuid = newUuid, content = EditChapterEvent' content1} :: KnowledgeModelEvent)
        else return event
  where
    unwrapEventChildUuids callback =
      case content.questionUuids of
        NothingChanged -> return event
        ChangedValue uuids -> callback uuids
    childUuidsFromKM :: KnowledgeModel -> [U.UUID]
    childUuidsFromKM km = getQuestionUuidsForChapterUuid km event.entityUuid
    isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
    isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
    resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
    resultUuids km childUuidsFromEvent =
      filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
sanitizeEditQuestionEvent :: KnowledgeModelMigration -> KnowledgeModelEvent -> EditQuestionEvent -> AppContextM KnowledgeModelEvent
sanitizeEditQuestionEvent state event content =
  unwrapKM state event $ \km -> do
    content1 <- applyExpertChange km content
    content2 <- applyReferenceChange km content1
    content3 <- applyAnswerChange km content2
    content4 <- applyItemTemplateQuestionChange km content3
    if content /= content4
      then do
        newUuid <- liftIO generateUuid
        return (event {uuid = newUuid, content = EditQuestionEvent' content4} :: KnowledgeModelEvent)
      else return event
  where
    -- ------------------------
    -- Answer Item Template
    -- ------------------------
    applyItemTemplateQuestionChange km (EditListQuestionEvent' content) =
      unwrapEventItemTemplateQuestionUuids $ \itqUuids ->
        return . EditListQuestionEvent' $ content {itemTemplateQuestionUuids = ChangedValue (resultUuids km itqUuids)}
      where
        unwrapEventItemTemplateQuestionUuids callback =
          case content.itemTemplateQuestionUuids of
            NothingChanged -> return . EditListQuestionEvent' $ content
            ChangedValue itqUuids -> callback itqUuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = getItemTemplateQuestionUuidsForQuestionUuid km event.entityUuid
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
    applyItemTemplateQuestionChange km event = return event
    -- ------------------------
    -- Answers
    -- ------------------------
    applyAnswerChange km (EditOptionsQuestionEvent' content) =
      unwrapEventChildUuids $ \childUuidsFromEvent ->
        return . EditOptionsQuestionEvent' $ content {answerUuids = ChangedValue (resultUuids km childUuidsFromEvent)}
      where
        unwrapEventChildUuids callback =
          case content.answerUuids of
            NothingChanged -> return . EditOptionsQuestionEvent' $ content
            ChangedValue uuids -> callback uuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = getAnswerUuidsForQuestionUuid km event.entityUuid
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
    applyAnswerChange km event = return event
    -- ------------------------
    -- References
    -- ------------------------
    applyReferenceChange km content =
      unwrapEventChildUuids $ \childUuidsFromEvent ->
        return . setReferenceUuids content $ ChangedValue (resultUuids km childUuidsFromEvent)
      where
        unwrapEventChildUuids callback =
          case getReferenceUuids content of
            NothingChanged -> return content
            ChangedValue uuids -> callback uuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = getReferenceUuidsForQuestionUuid km event.entityUuid
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km
    -- ------------------------
    -- Experts
    -- ------------------------
    applyExpertChange km content =
      unwrapEventChildUuids $ \childUuidsFromEvent ->
        return . setExpertUuids content $ ChangedValue (resultUuids km childUuidsFromEvent)
      where
        unwrapEventChildUuids callback =
          case getExpertUuids content of
            NothingChanged -> return content
            ChangedValue uuids -> callback uuids
        childUuidsFromKM :: KnowledgeModel -> [U.UUID]
        childUuidsFromKM km = getExpertUuidsForQuestionUuid km event.entityUuid
        isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
        isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
        resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
        resultUuids km childUuidsFromEvent =
          filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

sanitizeEditAnswerEvent :: KnowledgeModelMigration -> KnowledgeModelEvent -> EditAnswerEvent -> AppContextM KnowledgeModelEvent
sanitizeEditAnswerEvent state event content =
  unwrapKM state event $ \km ->
    unwrapEventChildUuids $ \childUuidsFromEvent -> do
      let content1 = content {followUpUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditAnswerEvent
      if content /= content1
        then do
          newUuid <- liftIO generateUuid
          return (event {uuid = newUuid, content = EditAnswerEvent' content1} :: KnowledgeModelEvent)
        else return event
  where
    unwrapEventChildUuids callback =
      case content.followUpUuids of
        NothingChanged -> return event
        ChangedValue uuids -> callback uuids
    childUuidsFromKM :: KnowledgeModel -> [U.UUID]
    childUuidsFromKM km = getQuestionUuidsForAnswerUuid km event.entityUuid
    isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
    isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
    resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
    resultUuids km childUuidsFromEvent =
      filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
-- ------------------------------------------------------------
unwrapKM state event callback =
  case state.currentKnowledgeModel of
    Nothing -> return event
    Just km -> callback km

changeEventUuid setter event = do
  uuid <- liftIO generateUuid
  return $ setter event uuid
