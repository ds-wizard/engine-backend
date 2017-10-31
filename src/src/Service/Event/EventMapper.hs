module Service.Event.EventMapper where

import Api.Resources.Event.EventDTO
import Model.Event.Event
import Service.Event.EventToDTO
import Service.Event.EventFromDTO

toDTOFn :: Event -> EventDTO
toDTOFn (MkEvent event) = toDTO event

toDTOs :: [Event] -> [EventDTO]
toDTOs = fmap toDTOFn

fromDTOFn :: EventDTO  -> Event
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
fromDTOFn (AddFollowUpQuestionEventDTO' event) = fromDTO event
fromDTOFn (EditFollowUpQuestionEventDTO' event) = fromDTO event
fromDTOFn (DeleteFollowUpQuestionEventDTO' event) = fromDTO event

fromDTOs :: [EventDTO] -> [Event]
fromDTOs = fmap fromDTOFn
