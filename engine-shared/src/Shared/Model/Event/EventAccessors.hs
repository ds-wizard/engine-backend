module Shared.Model.Event.EventAccessors where

import qualified Data.UUID as U

import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Event
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent

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
  getEventUuid = _addKnowledgeModelEventUuid
  getEventParentUuid = _addKnowledgeModelEventParentUuid
  getEventNodeUuid = _addKnowledgeModelEventEntityUuid

instance EventAccesors EditKnowledgeModelEvent where
  getEventUuid = _editKnowledgeModelEventUuid
  getEventParentUuid = _editKnowledgeModelEventParentUuid
  getEventNodeUuid = _editKnowledgeModelEventEntityUuid

instance EventAccesors AddChapterEvent where
  getEventUuid = _addChapterEventUuid
  getEventParentUuid = _addChapterEventParentUuid
  getEventNodeUuid = _addChapterEventEntityUuid

instance EventAccesors EditChapterEvent where
  getEventUuid = _editChapterEventUuid
  getEventParentUuid = _editChapterEventParentUuid
  getEventNodeUuid = _editChapterEventEntityUuid

instance EventAccesors DeleteChapterEvent where
  getEventUuid = _deleteChapterEventUuid
  getEventParentUuid = _deleteChapterEventParentUuid
  getEventNodeUuid = _deleteChapterEventEntityUuid

instance EventAccesors AddQuestionEvent where
  getEventUuid (AddOptionsQuestionEvent' event) = _addOptionsQuestionEventUuid event
  getEventUuid (AddListQuestionEvent' event) = _addListQuestionEventUuid event
  getEventUuid (AddValueQuestionEvent' event) = _addValueQuestionEventUuid event
  getEventUuid (AddIntegrationQuestionEvent' event) = _addIntegrationQuestionEventUuid event
  getEventParentUuid (AddOptionsQuestionEvent' event) = _addOptionsQuestionEventParentUuid event
  getEventParentUuid (AddListQuestionEvent' event) = _addListQuestionEventParentUuid event
  getEventParentUuid (AddValueQuestionEvent' event) = _addValueQuestionEventParentUuid event
  getEventParentUuid (AddIntegrationQuestionEvent' event) = _addIntegrationQuestionEventParentUuid event
  getEventNodeUuid (AddOptionsQuestionEvent' event) = _addOptionsQuestionEventEntityUuid event
  getEventNodeUuid (AddListQuestionEvent' event) = _addListQuestionEventEntityUuid event
  getEventNodeUuid (AddValueQuestionEvent' event) = _addValueQuestionEventEntityUuid event
  getEventNodeUuid (AddIntegrationQuestionEvent' event) = _addIntegrationQuestionEventEntityUuid event

instance EventAccesors EditQuestionEvent where
  getEventUuid (EditOptionsQuestionEvent' event) = _editOptionsQuestionEventUuid event
  getEventUuid (EditListQuestionEvent' event) = _editListQuestionEventUuid event
  getEventUuid (EditValueQuestionEvent' event) = _editValueQuestionEventUuid event
  getEventUuid (EditIntegrationQuestionEvent' event) = _editIntegrationQuestionEventUuid event
  getEventParentUuid (EditOptionsQuestionEvent' event) = _editOptionsQuestionEventParentUuid event
  getEventParentUuid (EditListQuestionEvent' event) = _editListQuestionEventParentUuid event
  getEventParentUuid (EditValueQuestionEvent' event) = _editValueQuestionEventParentUuid event
  getEventParentUuid (EditIntegrationQuestionEvent' event) = _editIntegrationQuestionEventParentUuid event
  getEventNodeUuid (EditOptionsQuestionEvent' event) = _editOptionsQuestionEventEntityUuid event
  getEventNodeUuid (EditListQuestionEvent' event) = _editListQuestionEventEntityUuid event
  getEventNodeUuid (EditValueQuestionEvent' event) = _editValueQuestionEventEntityUuid event
  getEventNodeUuid (EditIntegrationQuestionEvent' event) = _editIntegrationQuestionEventEntityUuid event

instance EventAccesors DeleteQuestionEvent where
  getEventUuid = _deleteQuestionEventUuid
  getEventParentUuid = _deleteQuestionEventParentUuid
  getEventNodeUuid = _deleteQuestionEventEntityUuid

instance EventAccesors AddAnswerEvent where
  getEventUuid = _addAnswerEventUuid
  getEventParentUuid = _addAnswerEventParentUuid
  getEventNodeUuid = _addAnswerEventEntityUuid

instance EventAccesors EditAnswerEvent where
  getEventUuid = _editAnswerEventUuid
  getEventParentUuid = _editAnswerEventParentUuid
  getEventNodeUuid = _editAnswerEventEntityUuid

instance EventAccesors DeleteAnswerEvent where
  getEventUuid = _deleteAnswerEventUuid
  getEventParentUuid = _deleteAnswerEventParentUuid
  getEventNodeUuid = _deleteAnswerEventEntityUuid

instance EventAccesors AddExpertEvent where
  getEventUuid = _addExpertEventUuid
  getEventParentUuid = _addExpertEventParentUuid
  getEventNodeUuid = _addExpertEventEntityUuid

instance EventAccesors EditExpertEvent where
  getEventUuid = _editExpertEventUuid
  getEventParentUuid = _editExpertEventParentUuid
  getEventNodeUuid = _editExpertEventEntityUuid

instance EventAccesors DeleteExpertEvent where
  getEventUuid = _deleteExpertEventUuid
  getEventParentUuid = _deleteExpertEventParentUuid
  getEventNodeUuid = _deleteExpertEventEntityUuid

instance EventAccesors AddReferenceEvent where
  getEventUuid (AddResourcePageReferenceEvent' event) = _addResourcePageReferenceEventUuid event
  getEventUuid (AddURLReferenceEvent' event) = _addURLReferenceEventUuid event
  getEventUuid (AddCrossReferenceEvent' event) = _addCrossReferenceEventUuid event
  getEventParentUuid (AddResourcePageReferenceEvent' event) = _addResourcePageReferenceEventParentUuid event
  getEventParentUuid (AddURLReferenceEvent' event) = _addURLReferenceEventParentUuid event
  getEventParentUuid (AddCrossReferenceEvent' event) = _addCrossReferenceEventParentUuid event
  getEventNodeUuid (AddResourcePageReferenceEvent' event) = _addResourcePageReferenceEventEntityUuid event
  getEventNodeUuid (AddURLReferenceEvent' event) = _addURLReferenceEventEntityUuid event
  getEventNodeUuid (AddCrossReferenceEvent' event) = _addCrossReferenceEventEntityUuid event

instance EventAccesors EditReferenceEvent where
  getEventUuid (EditResourcePageReferenceEvent' event) = _editResourcePageReferenceEventUuid event
  getEventUuid (EditURLReferenceEvent' event) = _editURLReferenceEventUuid event
  getEventUuid (EditCrossReferenceEvent' event) = _editCrossReferenceEventUuid event
  getEventParentUuid (EditResourcePageReferenceEvent' event) = _editResourcePageReferenceEventParentUuid event
  getEventParentUuid (EditURLReferenceEvent' event) = _editURLReferenceEventParentUuid event
  getEventParentUuid (EditCrossReferenceEvent' event) = _editCrossReferenceEventParentUuid event
  getEventNodeUuid (EditResourcePageReferenceEvent' event) = _editResourcePageReferenceEventEntityUuid event
  getEventNodeUuid (EditURLReferenceEvent' event) = _editURLReferenceEventEntityUuid event
  getEventNodeUuid (EditCrossReferenceEvent' event) = _editCrossReferenceEventEntityUuid event

instance EventAccesors DeleteReferenceEvent where
  getEventUuid = _deleteReferenceEventUuid
  getEventParentUuid = _deleteReferenceEventParentUuid
  getEventNodeUuid = _deleteReferenceEventEntityUuid

instance EventAccesors AddTagEvent where
  getEventUuid = _addTagEventUuid
  getEventParentUuid = _addTagEventParentUuid
  getEventNodeUuid = _addTagEventEntityUuid

instance EventAccesors EditTagEvent where
  getEventUuid = _editTagEventUuid
  getEventParentUuid = _editTagEventParentUuid
  getEventNodeUuid = _editTagEventEntityUuid

instance EventAccesors DeleteTagEvent where
  getEventUuid = _deleteTagEventUuid
  getEventParentUuid = _deleteTagEventParentUuid
  getEventNodeUuid = _deleteTagEventEntityUuid

instance EventAccesors AddIntegrationEvent where
  getEventUuid = _addIntegrationEventUuid
  getEventParentUuid = _addIntegrationEventParentUuid
  getEventNodeUuid = _addIntegrationEventEntityUuid

instance EventAccesors EditIntegrationEvent where
  getEventUuid = _editIntegrationEventUuid
  getEventParentUuid = _editIntegrationEventParentUuid
  getEventNodeUuid = _editIntegrationEventEntityUuid

instance EventAccesors DeleteIntegrationEvent where
  getEventUuid = _deleteIntegrationEventUuid
  getEventParentUuid = _deleteIntegrationEventParentUuid
  getEventNodeUuid = _deleteIntegrationEventEntityUuid
