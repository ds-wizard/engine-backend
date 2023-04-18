module Wizard.Service.Migration.KnowledgeModel.Migrator.Sanitizator where

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.UUID as U

import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors

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
      if event /= event3
        then changeEventUuid (\e newValue -> e {uuid = newValue} :: EditKnowledgeModelEvent) event3
        else return event
    where
      -- ------------------------
      -- Chapter
      -- ------------------------

      applyChapterChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent -> do
          let result = event {chapterUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditKnowledgeModelEvent
          return result
        where
          unwrapEventChildUuids callback =
            case event.chapterUuids of
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
instance Sanitizator EditChapterEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childUuidsFromEvent -> do
        let event1 = event {questionUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditChapterEvent
        if event /= event1
          then changeEventUuid (\e newValue -> e {uuid = newValue} :: EditChapterEvent) event1
          else return event
    where
      unwrapEventChildUuids callback =
        case event.questionUuids of
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
instance Sanitizator EditQuestionEvent where
  sanitize state event =
    unwrapKM state event $ \km -> do
      event1 <- applyExpertChange km event
      event2 <- applyReferenceChange km event1
      event3 <- applyAnswerChange km event2
      event4 <- applyItemTemplateQuestionChange km event3
      if event /= event4
        then changeEventUuid setUuid event4
        else return event
    where
      -- ------------------------
      -- Answer Item Template
      -- ------------------------

      applyItemTemplateQuestionChange km (EditListQuestionEvent' event) =
        unwrapEventItemTemplateQuestionUuids $ \itqUuids ->
          return . EditListQuestionEvent' $ event {itemTemplateQuestionUuids = ChangedValue (resultUuids km itqUuids)}
        where
          unwrapEventItemTemplateQuestionUuids callback =
            case event.itemTemplateQuestionUuids of
              NothingChanged -> return . EditListQuestionEvent' $ event
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
      applyAnswerChange km (EditOptionsQuestionEvent' event) =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return . EditOptionsQuestionEvent' $ event {answerUuids = ChangedValue (resultUuids km childUuidsFromEvent)}
        where
          unwrapEventChildUuids callback =
            case event.answerUuids of
              NothingChanged -> return . EditOptionsQuestionEvent' $ event
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
      applyReferenceChange km event =
        unwrapEventChildUuids $ \childUuidsFromEvent ->
          return . setReferenceUuids event $ ChangedValue (resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case getReferenceUuids event of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getReferenceUuidsForQuestionUuid km (getEntityUuid event)
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
          return . setExpertUuids event $ ChangedValue (resultUuids km childUuidsFromEvent)
        where
          unwrapEventChildUuids callback =
            case getExpertUuids event of
              NothingChanged -> return event
              ChangedValue uuids -> callback uuids
          childUuidsFromKM :: KnowledgeModel -> [U.UUID]
          childUuidsFromKM km = getExpertUuidsForQuestionUuid km (getEntityUuid event)
          isInChildUuids :: KnowledgeModel -> U.UUID -> Bool
          isInChildUuids km uuid = isJust $ find (== uuid) (childUuidsFromKM km)
          resultUuids :: KnowledgeModel -> [U.UUID] -> [U.UUID]
          resultUuids km childUuidsFromEvent =
            filter (isInChildUuids km) $ removeDuplicates $ childUuidsFromEvent ++ childUuidsFromKM km

-- ------------------------------------------------------------
instance Sanitizator EditAnswerEvent where
  sanitize state event =
    unwrapKM state event $ \km ->
      unwrapEventChildUuids $ \childUuidsFromEvent -> do
        let event1 = event {followUpUuids = ChangedValue (resultUuids km childUuidsFromEvent)} :: EditAnswerEvent
        if event /= event1
          then changeEventUuid (\e newValue -> e {uuid = newValue} :: EditAnswerEvent) event1
          else return event
    where
      unwrapEventChildUuids callback =
        case event.followUpUuids of
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
  uuid <- generateUuid
  return $ setter event uuid
