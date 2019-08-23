module Service.Event.EventMapper where

import Api.Resource.Event.EventDTO
import Model.Event.Event
import Service.Event.EventFromDTO
import Service.Event.EventToDTO

toDTOFn :: Event -> EventDTO
toDTOFn (AddKnowledgeModelEvent' event) = toDTO event
toDTOFn (EditKnowledgeModelEvent' event) = toDTO event
toDTOFn (AddChapterEvent' event) = toDTO event
toDTOFn (EditChapterEvent' event) = toDTO event
toDTOFn (DeleteChapterEvent' event) = toDTO event
toDTOFn (AddQuestionEvent' event) = toDTO event
toDTOFn (EditQuestionEvent' event) = toDTO event
toDTOFn (DeleteQuestionEvent' event) = toDTO event
toDTOFn (AddAnswerEvent' event) = toDTO event
toDTOFn (EditAnswerEvent' event) = toDTO event
toDTOFn (DeleteAnswerEvent' event) = toDTO event
toDTOFn (AddExpertEvent' event) = toDTO event
toDTOFn (EditExpertEvent' event) = toDTO event
toDTOFn (DeleteExpertEvent' event) = toDTO event
toDTOFn (AddReferenceEvent' event) = toDTO event
toDTOFn (EditReferenceEvent' event) = toDTO event
toDTOFn (DeleteReferenceEvent' event) = toDTO event
toDTOFn (AddTagEvent' event) = toDTO event
toDTOFn (EditTagEvent' event) = toDTO event
toDTOFn (DeleteTagEvent' event) = toDTO event
toDTOFn (AddIntegrationEvent' event) = toDTO event
toDTOFn (EditIntegrationEvent' event) = toDTO event
toDTOFn (DeleteIntegrationEvent' event) = toDTO event

toDTOs :: [Event] -> [EventDTO]
toDTOs = fmap toDTOFn

fromDTOFn :: EventDTO -> Event
fromDTOFn (AddKnowledgeModelEventDTO' event) = fromDTO event
fromDTOFn (EditKnowledgeModelEventDTO' event) = fromDTO event
fromDTOFn (AddChapterEventDTO' event) = fromDTO event
fromDTOFn (EditChapterEventDTO' event) = fromDTO event
fromDTOFn (DeleteChapterEventDTO' event) = fromDTO event
fromDTOFn (AddQuestionEventDTO' event) = fromDTO event
fromDTOFn (EditQuestionEventDTO' event) = fromDTO event
fromDTOFn (DeleteQuestionEventDTO' event) = fromDTO event
fromDTOFn (AddAnswerEventDTO' event) = fromDTO event
fromDTOFn (EditAnswerEventDTO' event) = fromDTO event
fromDTOFn (DeleteAnswerEventDTO' event) = fromDTO event
fromDTOFn (AddExpertEventDTO' event) = fromDTO event
fromDTOFn (EditExpertEventDTO' event) = fromDTO event
fromDTOFn (DeleteExpertEventDTO' event) = fromDTO event
fromDTOFn (AddReferenceEventDTO' event) = fromDTO event
fromDTOFn (EditReferenceEventDTO' event) = fromDTO event
fromDTOFn (DeleteReferenceEventDTO' event) = fromDTO event
fromDTOFn (AddTagEventDTO' event) = fromDTO event
fromDTOFn (EditTagEventDTO' event) = fromDTO event
fromDTOFn (DeleteTagEventDTO' event) = fromDTO event
fromDTOFn (AddIntegrationEventDTO' event) = fromDTO event
fromDTOFn (EditIntegrationEventDTO' event) = fromDTO event
fromDTOFn (DeleteIntegrationEventDTO' event) = fromDTO event

fromDTOs :: [EventDTO] -> [Event]
fromDTOs = fmap fromDTOFn
