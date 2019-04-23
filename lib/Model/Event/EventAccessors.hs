module Model.Event.EventAccessors where

import Control.Lens
import qualified Data.UUID as U

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.EventField
import Model.Event.EventPath
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent

isAddAction :: Event -> Bool
isAddAction (AddKnowledgeModelEvent' _) = True
isAddAction (AddChapterEvent' _) = True
isAddAction (AddQuestionEvent' _) = True
isAddAction (AddAnswerEvent' _) = True
isAddAction (AddExpertEvent' _) = True
isAddAction (AddReferenceEvent' _) = True
isAddAction (AddTagEvent' _) = True
isAddAction (AddIntegrationEvent' _) = True
isAddAction _ = False

isEditAction :: Event -> Bool
isEditAction (EditKnowledgeModelEvent' _) = True
isEditAction (EditChapterEvent' _) = True
isEditAction (EditQuestionEvent' _) = True
isEditAction (EditAnswerEvent' _) = True
isEditAction (EditExpertEvent' _) = True
isEditAction (EditReferenceEvent' _) = True
isEditAction (EditTagEvent' _) = True
isEditAction (EditIntegrationEvent' _) = True
isEditAction _ = False

isDeleteAction :: Event -> Bool
isDeleteAction (DeleteChapterEvent' _) = True
isDeleteAction (DeleteQuestionEvent' _) = True
isDeleteAction (DeleteAnswerEvent' _) = True
isDeleteAction (DeleteExpertEvent' _) = True
isDeleteAction (DeleteReferenceEvent' _) = True
isDeleteAction (DeleteTagEvent' _) = True
isDeleteAction (DeleteIntegrationEvent' _) = True
isDeleteAction _ = False

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
  getPath :: a -> EventPath

instance EventAccesors AddKnowledgeModelEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. kmUuid
  getPath event = event ^. path

instance EventAccesors EditKnowledgeModelEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. kmUuid
  getPath event = event ^. path

instance EventAccesors AddChapterEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. chapterUuid
  getPath event = event ^. path

instance EventAccesors EditChapterEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. chapterUuid
  getPath event = event ^. path

instance EventAccesors DeleteChapterEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. chapterUuid
  getPath event = event ^. path

instance EventAccesors AddQuestionEvent where
  getEventUuid (AddOptionsQuestionEvent' event) = event ^. uuid
  getEventUuid (AddListQuestionEvent' event) = event ^. uuid
  getEventUuid (AddValueQuestionEvent' event) = event ^. uuid
  getEventUuid (AddIntegrationQuestionEvent' event) = event ^. uuid
  getEventNodeUuid (AddOptionsQuestionEvent' event) = event ^. questionUuid
  getEventNodeUuid (AddListQuestionEvent' event) = event ^. questionUuid
  getEventNodeUuid (AddValueQuestionEvent' event) = event ^. questionUuid
  getEventNodeUuid (AddIntegrationQuestionEvent' event) = event ^. questionUuid
  getPath (AddOptionsQuestionEvent' e) = e ^. path
  getPath (AddListQuestionEvent' e) = e ^. path
  getPath (AddValueQuestionEvent' e) = e ^. path
  getPath (AddIntegrationQuestionEvent' e) = e ^. path

instance EventAccesors EditQuestionEvent where
  getEventUuid (EditOptionsQuestionEvent' event) = event ^. uuid
  getEventUuid (EditListQuestionEvent' event) = event ^. uuid
  getEventUuid (EditValueQuestionEvent' event) = event ^. uuid
  getEventUuid (EditIntegrationQuestionEvent' event) = event ^. uuid
  getEventNodeUuid (EditOptionsQuestionEvent' event) = event ^. questionUuid
  getEventNodeUuid (EditListQuestionEvent' event) = event ^. questionUuid
  getEventNodeUuid (EditValueQuestionEvent' event) = event ^. questionUuid
  getEventNodeUuid (EditIntegrationQuestionEvent' event) = event ^. questionUuid
  getPath (EditOptionsQuestionEvent' e) = e ^. path
  getPath (EditListQuestionEvent' e) = e ^. path
  getPath (EditValueQuestionEvent' e) = e ^. path
  getPath (EditIntegrationQuestionEvent' e) = e ^. path

instance EventAccesors DeleteQuestionEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. questionUuid
  getPath e = e ^. path

instance EventAccesors AddAnswerEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. answerUuid
  getPath event = event ^. path

instance EventAccesors EditAnswerEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. answerUuid
  getPath event = event ^. path

instance EventAccesors DeleteAnswerEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. answerUuid
  getPath event = event ^. path

instance EventAccesors AddExpertEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. expertUuid
  getPath event = event ^. path

instance EventAccesors EditExpertEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. expertUuid
  getPath event = event ^. path

instance EventAccesors DeleteExpertEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. expertUuid
  getPath event = event ^. path

instance EventAccesors AddReferenceEvent where
  getEventUuid (AddResourcePageReferenceEvent' event) = event ^. uuid
  getEventUuid (AddURLReferenceEvent' event) = event ^. uuid
  getEventUuid (AddCrossReferenceEvent' event) = event ^. uuid
  getEventNodeUuid (AddResourcePageReferenceEvent' event) = event ^. referenceUuid
  getEventNodeUuid (AddURLReferenceEvent' event) = event ^. referenceUuid
  getEventNodeUuid (AddCrossReferenceEvent' event) = event ^. referenceUuid
  getPath (AddResourcePageReferenceEvent' e) = e ^. path
  getPath (AddURLReferenceEvent' e) = e ^. path
  getPath (AddCrossReferenceEvent' e) = e ^. path

instance EventAccesors EditReferenceEvent where
  getEventUuid (EditResourcePageReferenceEvent' event) = event ^. uuid
  getEventUuid (EditURLReferenceEvent' event) = event ^. uuid
  getEventUuid (EditCrossReferenceEvent' event) = event ^. uuid
  getEventNodeUuid (EditResourcePageReferenceEvent' event) = event ^. referenceUuid
  getEventNodeUuid (EditURLReferenceEvent' event) = event ^. referenceUuid
  getEventNodeUuid (EditCrossReferenceEvent' event) = event ^. referenceUuid
  getPath (EditResourcePageReferenceEvent' e) = e ^. path
  getPath (EditURLReferenceEvent' e) = e ^. path
  getPath (EditCrossReferenceEvent' e) = e ^. path

instance EventAccesors DeleteReferenceEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. referenceUuid
  getPath e = e ^. path

instance EventAccesors AddTagEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. tagUuid
  getPath event = event ^. path

instance EventAccesors EditTagEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. tagUuid
  getPath event = event ^. path

instance EventAccesors DeleteTagEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. tagUuid
  getPath event = event ^. path

instance EventAccesors AddIntegrationEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. integrationUuid
  getPath event = event ^. path

instance EventAccesors EditIntegrationEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. integrationUuid
  getPath event = event ^. path

instance EventAccesors DeleteIntegrationEvent where
  getEventUuid event = event ^. uuid
  getEventNodeUuid event = event ^. integrationUuid
  getPath event = event ^. path

class QuestionEventAccessors a where
  getEventQuestionUuid :: a -> U.UUID
  getEventExpertUuids :: a -> EventField [U.UUID]
  getEventReferenceUuids :: a -> EventField [U.UUID]

instance QuestionEventAccessors AddQuestionEvent where
  getEventQuestionUuid (AddOptionsQuestionEvent' e) = e ^. questionUuid
  getEventQuestionUuid (AddListQuestionEvent' e) = e ^. questionUuid
  getEventQuestionUuid (AddValueQuestionEvent' e) = e ^. questionUuid
  getEventQuestionUuid (AddIntegrationQuestionEvent' e) = e ^. questionUuid
  getEventExpertUuids _ = NothingChanged
  getEventReferenceUuids _ = NothingChanged

instance QuestionEventAccessors EditQuestionEvent where
  getEventQuestionUuid (EditOptionsQuestionEvent' e) = e ^. questionUuid
  getEventQuestionUuid (EditListQuestionEvent' e) = e ^. questionUuid
  getEventQuestionUuid (EditValueQuestionEvent' e) = e ^. questionUuid
  getEventQuestionUuid (EditIntegrationQuestionEvent' e) = e ^. questionUuid
  getEventReferenceUuids (EditOptionsQuestionEvent' e) = e ^. referenceUuids
  getEventReferenceUuids (EditListQuestionEvent' e) = e ^. referenceUuids
  getEventReferenceUuids (EditValueQuestionEvent' e) = e ^. referenceUuids
  getEventReferenceUuids (EditIntegrationQuestionEvent' e) = e ^. referenceUuids
  getEventExpertUuids (EditOptionsQuestionEvent' e) = e ^. expertUuids
  getEventExpertUuids (EditListQuestionEvent' e) = e ^. expertUuids
  getEventExpertUuids (EditValueQuestionEvent' e) = e ^. expertUuids
  getEventExpertUuids (EditIntegrationQuestionEvent' e) = e ^. expertUuids

instance QuestionEventAccessors DeleteQuestionEvent where
  getEventQuestionUuid e = e ^. questionUuid
  getEventExpertUuids _ = NothingChanged
  getEventReferenceUuids _ = NothingChanged

eqChangeEventUuid :: (U.UUID -> Identity U.UUID) -> EditQuestionEvent -> Identity EditQuestionEvent
eqChangeEventUuid convert e = Identity . updateEvent $ e
  where
    newValue :: U.UUID
    newValue = runIdentity . convert $ getEventUuid e
    updateEvent :: EditQuestionEvent -> EditQuestionEvent
    updateEvent (EditOptionsQuestionEvent' e) = EditOptionsQuestionEvent' $ e & uuid .~ newValue
    updateEvent (EditListQuestionEvent' e) = EditListQuestionEvent' $ e & uuid .~ newValue
    updateEvent (EditValueQuestionEvent' e) = EditValueQuestionEvent' $ e & uuid .~ newValue
    updateEvent (EditIntegrationQuestionEvent' e) = EditIntegrationQuestionEvent' $ e & uuid .~ newValue

eqChangeExpertUuids ::
     (EventField [U.UUID] -> Identity (EventField [U.UUID])) -> EditQuestionEvent -> Identity EditQuestionEvent
eqChangeExpertUuids convert e = Identity . updateEvent $ e
  where
    newValue :: EventField [U.UUID]
    newValue = runIdentity . convert $ NothingChanged
    updateEvent :: EditQuestionEvent -> EditQuestionEvent
    updateEvent (EditOptionsQuestionEvent' e) = EditOptionsQuestionEvent' $ e & expertUuids .~ newValue
    updateEvent (EditListQuestionEvent' e) = EditListQuestionEvent' $ e & expertUuids .~ newValue
    updateEvent (EditValueQuestionEvent' e) = EditValueQuestionEvent' $ e & expertUuids .~ newValue
    updateEvent (EditIntegrationQuestionEvent' e) = EditIntegrationQuestionEvent' $ e & expertUuids .~ newValue

eqChangeReferenceUuids ::
     (EventField [U.UUID] -> Identity (EventField [U.UUID])) -> EditQuestionEvent -> Identity EditQuestionEvent
eqChangeReferenceUuids convert e = Identity . updateEvent $ e
  where
    newValue :: EventField [U.UUID]
    newValue = runIdentity . convert $ NothingChanged
    updateEvent :: EditQuestionEvent -> EditQuestionEvent
    updateEvent (EditOptionsQuestionEvent' e) = EditOptionsQuestionEvent' $ e & referenceUuids .~ newValue
    updateEvent (EditListQuestionEvent' e) = EditListQuestionEvent' $ e & referenceUuids .~ newValue
    updateEvent (EditValueQuestionEvent' e) = EditValueQuestionEvent' $ e & referenceUuids .~ newValue
    updateEvent (EditIntegrationQuestionEvent' e) = EditIntegrationQuestionEvent' $ e & referenceUuids .~ newValue
