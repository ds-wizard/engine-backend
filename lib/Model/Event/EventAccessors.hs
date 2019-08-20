module Model.Event.EventAccessors where

import Control.Lens
import qualified Data.UUID as U

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent

getEventUuid' :: Event -> U.UUID
getEventUuid' (AddKnowledgeModelEvent' event) = getEventUuid event
getEventUuid' (EditKnowledgeModelEvent' event) = getEventUuid event
getEventUuid' (AddChapterEvent' event) = getEventUuid event
getEventUuid' (EditChapterEvent' event) = getEventUuid event
getEventUuid' (DeleteChapterEvent' event) = getEventUuid event
getEventUuid' (AddQuestionEvent' event) = getEventUuid event
getEventUuid' (EditQuestionEvent' event) = getEventUuid event
getEventUuid' (DeleteQuestionEvent' event) = getEventUuid event
getEventUuid' (AddAnswerEvent' event) = getEventUuid event
getEventUuid' (EditAnswerEvent' event) = getEventUuid event
getEventUuid' (DeleteAnswerEvent' event) = getEventUuid event
getEventUuid' (AddExpertEvent' event) = getEventUuid event
getEventUuid' (EditExpertEvent' event) = getEventUuid event
getEventUuid' (DeleteExpertEvent' event) = getEventUuid event
getEventUuid' (AddReferenceEvent' event) = getEventUuid event
getEventUuid' (EditReferenceEvent' event) = getEventUuid event
getEventUuid' (DeleteReferenceEvent' event) = getEventUuid event
getEventUuid' (AddTagEvent' event) = getEventUuid event
getEventUuid' (EditTagEvent' event) = getEventUuid event
getEventUuid' (DeleteTagEvent' event) = getEventUuid event
getEventUuid' (AddIntegrationEvent' event) = getEventUuid event
getEventUuid' (EditIntegrationEvent' event) = getEventUuid event
getEventUuid' (DeleteIntegrationEvent' event) = getEventUuid event

class EventAccesors a where
  getEventUuid :: a -> U.UUID
  getEventNodeUuid :: a -> U.UUID
  getEventParentUuid :: a -> U.UUID

instance EventAccesors AddKnowledgeModelEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors EditKnowledgeModelEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors AddChapterEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors EditChapterEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors DeleteChapterEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors AddQuestionEvent where
  getEventUuid (AddOptionsQuestionEvent' event) = event ^. uuid
  getEventUuid (AddListQuestionEvent' event) = event ^. uuid
  getEventUuid (AddValueQuestionEvent' event) = event ^. uuid
  getEventUuid (AddIntegrationQuestionEvent' event) = event ^. uuid
  getEventNodeUuid (AddOptionsQuestionEvent' event) = event ^. entityUuid
  getEventNodeUuid (AddListQuestionEvent' event) = event ^. entityUuid
  getEventNodeUuid (AddValueQuestionEvent' event) = event ^. entityUuid
  getEventNodeUuid (AddIntegrationQuestionEvent' event) = event ^. entityUuid
  getEventParentUuid (AddOptionsQuestionEvent' e) = e ^. parentUuid
  getEventParentUuid (AddListQuestionEvent' e) = e ^. parentUuid
  getEventParentUuid (AddValueQuestionEvent' e) = e ^. parentUuid
  getEventParentUuid (AddIntegrationQuestionEvent' e) = e ^. parentUuid

instance EventAccesors EditQuestionEvent where
  getEventUuid (EditOptionsQuestionEvent' event) = event ^. uuid
  getEventUuid (EditListQuestionEvent' event) = event ^. uuid
  getEventUuid (EditValueQuestionEvent' event) = event ^. uuid
  getEventUuid (EditIntegrationQuestionEvent' event) = event ^. uuid
  getEventNodeUuid (EditOptionsQuestionEvent' event) = event ^. entityUuid
  getEventNodeUuid (EditListQuestionEvent' event) = event ^. entityUuid
  getEventNodeUuid (EditValueQuestionEvent' event) = event ^. entityUuid
  getEventNodeUuid (EditIntegrationQuestionEvent' event) = event ^. entityUuid
  getEventParentUuid (EditOptionsQuestionEvent' e) = e ^. parentUuid
  getEventParentUuid (EditListQuestionEvent' e) = e ^. parentUuid
  getEventParentUuid (EditValueQuestionEvent' e) = e ^. parentUuid
  getEventParentUuid (EditIntegrationQuestionEvent' e) = e ^. parentUuid

instance EventAccesors DeleteQuestionEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid e = e ^. parentUuid

instance EventAccesors AddAnswerEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors EditAnswerEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors DeleteAnswerEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors AddExpertEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors EditExpertEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors DeleteExpertEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors AddReferenceEvent where
  getEventUuid (AddResourcePageReferenceEvent' event) = event ^. uuid
  getEventUuid (AddURLReferenceEvent' event) = event ^. uuid
  getEventUuid (AddCrossReferenceEvent' event) = event ^. uuid
  getEventNodeUuid (AddResourcePageReferenceEvent' event) = event ^. entityUuid
  getEventNodeUuid (AddURLReferenceEvent' event) = event ^. entityUuid
  getEventNodeUuid (AddCrossReferenceEvent' event) = event ^. entityUuid
  getEventParentUuid (AddResourcePageReferenceEvent' e) = e ^. parentUuid
  getEventParentUuid (AddURLReferenceEvent' e) = e ^. parentUuid
  getEventParentUuid (AddCrossReferenceEvent' e) = e ^. parentUuid

instance EventAccesors EditReferenceEvent where
  getEventUuid (EditResourcePageReferenceEvent' event) = event ^. uuid
  getEventUuid (EditURLReferenceEvent' event) = event ^. uuid
  getEventUuid (EditCrossReferenceEvent' event) = event ^. uuid
  getEventNodeUuid (EditResourcePageReferenceEvent' event) = event ^. entityUuid
  getEventNodeUuid (EditURLReferenceEvent' event) = event ^. entityUuid
  getEventNodeUuid (EditCrossReferenceEvent' event) = event ^. entityUuid
  getEventParentUuid (EditResourcePageReferenceEvent' e) = e ^. parentUuid
  getEventParentUuid (EditURLReferenceEvent' e) = e ^. parentUuid
  getEventParentUuid (EditCrossReferenceEvent' e) = e ^. parentUuid

instance EventAccesors DeleteReferenceEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid e = e ^. parentUuid

instance EventAccesors AddTagEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors EditTagEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors DeleteTagEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors AddIntegrationEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors EditIntegrationEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid

instance EventAccesors DeleteIntegrationEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. entityUuid
  getEventParentUuid event = event ^. parentUuid
