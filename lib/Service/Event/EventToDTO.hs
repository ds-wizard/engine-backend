module Service.Event.EventToDTO where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.EventPath
import Model.Event.Expert.ExpertEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.KnowledgeModelMapper

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class EventToDTO a where
  toDTO :: a -> EventDTO

toEventFieldDTO :: EventField a -> EventFieldDTO a
toEventFieldDTO (ChangedValue value) = ChangedValueDTO value
toEventFieldDTO NothingChanged = NothingChangedDTO

toEventPathItemDTO :: EventPathItem -> EventPathItemDTO
toEventPathItemDTO dto = EventPathItemDTO {_eventPathItemDTOPType = dto ^. pType, _eventPathItemDTOUuid = dto ^. uuid}

toEventPathDTO :: EventPath -> EventPathDTO
toEventPathDTO dto = toEventPathItemDTO <$> dto

toEventFieldAndAnswerItemTemplate ::
     EventField (Maybe AnswerItemTemplate) -> EventFieldDTO (Maybe AnswerItemTemplateDTO)
toEventFieldAndAnswerItemTemplate efMaybeAit =
  case efMaybeAit of
    ChangedValue maybeAit -> ChangedValueDTO $ toAnswerItemTemplateDTO <$> maybeAit
    NothingChanged -> NothingChangedDTO

toEventFieldAndAnswerItemTemplatePlain ::
     EventField (Maybe AnswerItemTemplatePlain) -> EventFieldDTO (Maybe AnswerItemTemplatePlainDTO)
toEventFieldAndAnswerItemTemplatePlain efMaybeAit =
  case efMaybeAit of
    ChangedValue maybeAit -> ChangedValueDTO $ toAnswerItemTemplatePlainDTO <$> maybeAit
    NothingChanged -> NothingChangedDTO

toEventFieldAndAnswerItemTemplatePlainWithIds ::
     EventField (Maybe AnswerItemTemplatePlainWithIds) -> EventFieldDTO (Maybe AnswerItemTemplatePlainWithIdsDTO)
toEventFieldAndAnswerItemTemplatePlainWithIds efMaybeAit =
  case efMaybeAit of
    ChangedValue maybeAit -> ChangedValueDTO $ toAnswerItemTemplatePlainWithIdsDTO <$> maybeAit
    NothingChanged -> NothingChangedDTO

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventToDTO AddKnowledgeModelEvent where
  toDTO event =
    AddKnowledgeModelEventDTO'
      AddKnowledgeModelEventDTO
      { _addKnowledgeModelEventDTOUuid = event ^. uuid
      , _addKnowledgeModelEventDTOPath = toEventPathDTO $ event ^. path
      , _addKnowledgeModelEventDTOKmUuid = event ^. kmUuid
      , _addKnowledgeModelEventDTOName = event ^. name
      }

instance EventToDTO EditKnowledgeModelEvent where
  toDTO event =
    EditKnowledgeModelEventDTO'
      EditKnowledgeModelEventDTO
      { _editKnowledgeModelEventDTOUuid = event ^. uuid
      , _editKnowledgeModelEventDTOPath = toEventPathDTO $ event ^. path
      , _editKnowledgeModelEventDTOKmUuid = event ^. kmUuid
      , _editKnowledgeModelEventDTOName = toEventFieldDTO $ event ^. name
      , _editKnowledgeModelEventDTOChapterIds = toEventFieldDTO $ event ^. chapterIds
      }

-------------------------
-- Chapter -----------------
-------------------------
instance EventToDTO AddChapterEvent where
  toDTO event =
    AddChapterEventDTO'
      AddChapterEventDTO
      { _addChapterEventDTOUuid = event ^. uuid
      , _addChapterEventDTOPath = toEventPathDTO $ event ^. path
      , _addChapterEventDTOChapterUuid = event ^. chapterUuid
      , _addChapterEventDTOTitle = event ^. title
      , _addChapterEventDTOText = event ^. text
      }

instance EventToDTO EditChapterEvent where
  toDTO event =
    EditChapterEventDTO'
      EditChapterEventDTO
      { _editChapterEventDTOUuid = event ^. uuid
      , _editChapterEventDTOPath = toEventPathDTO $ event ^. path
      , _editChapterEventDTOChapterUuid = event ^. chapterUuid
      , _editChapterEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editChapterEventDTOText = toEventFieldDTO $ event ^. text
      , _editChapterEventDTOQuestionIds = toEventFieldDTO $ event ^. questionIds
      }

instance EventToDTO DeleteChapterEvent where
  toDTO event =
    DeleteChapterEventDTO'
      DeleteChapterEventDTO
      { _deleteChapterEventDTOUuid = event ^. uuid
      , _deleteChapterEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteChapterEventDTOChapterUuid = event ^. chapterUuid
      }

-- -------------------------
-- Question ----------------
-- -------------------------
instance EventToDTO AddQuestionEvent where
  toDTO event =
    AddQuestionEventDTO'
      AddQuestionEventDTO
      { _addQuestionEventDTOUuid = event ^. uuid
      , _addQuestionEventDTOPath = toEventPathDTO $ event ^. path
      , _addQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _addQuestionEventDTOShortQuestionUuid = event ^. shortQuestionUuid
      , _addQuestionEventDTOQType = event ^. qType
      , _addQuestionEventDTOTitle = event ^. title
      , _addQuestionEventDTOText = event ^. text
      , _addQuestionEventDTOAnswerItemTemplatePlain = toAnswerItemTemplatePlainDTO <$> event ^. answerItemTemplatePlain
      }

instance EventToDTO EditQuestionEvent where
  toDTO event =
    EditQuestionEventDTO'
      EditQuestionEventDTO
      { _editQuestionEventDTOUuid = event ^. uuid
      , _editQuestionEventDTOPath = toEventPathDTO $ event ^. path
      , _editQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _editQuestionEventDTOShortQuestionUuid = toEventFieldDTO $ event ^. shortQuestionUuid
      , _editQuestionEventDTOQType = toEventFieldDTO $ event ^. qType
      , _editQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editQuestionEventDTOText = toEventFieldDTO $ event ^. text
      , _editQuestionEventDTOAnswerItemTemplatePlainWithIds =
          toEventFieldAndAnswerItemTemplatePlainWithIds $ event ^. answerItemTemplatePlainWithIds
      , _editQuestionEventDTOAnswerIds = toEventFieldDTO $ event ^. answerIds
      , _editQuestionEventDTOExpertIds = toEventFieldDTO $ event ^. expertIds
      , _editQuestionEventDTOReferenceIds = toEventFieldDTO $ event ^. referenceIds
      }

instance EventToDTO DeleteQuestionEvent where
  toDTO event =
    DeleteQuestionEventDTO'
      DeleteQuestionEventDTO
      { _deleteQuestionEventDTOUuid = event ^. uuid
      , _deleteQuestionEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteQuestionEventDTOQuestionUuid = event ^. questionUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventToDTO AddAnswerEvent where
  toDTO event =
    AddAnswerEventDTO'
      AddAnswerEventDTO
      { _addAnswerEventDTOUuid = event ^. uuid
      , _addAnswerEventDTOPath = toEventPathDTO $ event ^. path
      , _addAnswerEventDTOAnswerUuid = event ^. answerUuid
      , _addAnswerEventDTOLabel = event ^. label
      , _addAnswerEventDTOAdvice = event ^. advice
      }

instance EventToDTO EditAnswerEvent where
  toDTO event =
    EditAnswerEventDTO'
      EditAnswerEventDTO
      { _editAnswerEventDTOUuid = event ^. uuid
      , _editAnswerEventDTOPath = toEventPathDTO $ event ^. path
      , _editAnswerEventDTOAnswerUuid = event ^. answerUuid
      , _editAnswerEventDTOLabel = toEventFieldDTO $ event ^. label
      , _editAnswerEventDTOAdvice = toEventFieldDTO $ event ^. advice
      , _editAnswerEventDTOFollowUpIds = toEventFieldDTO $ event ^. followUpIds
      }

instance EventToDTO DeleteAnswerEvent where
  toDTO event =
    DeleteAnswerEventDTO'
      DeleteAnswerEventDTO
      { _deleteAnswerEventDTOUuid = event ^. uuid
      , _deleteAnswerEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteAnswerEventDTOAnswerUuid = event ^. answerUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventToDTO AddExpertEvent where
  toDTO event =
    AddExpertEventDTO'
      AddExpertEventDTO
      { _addExpertEventDTOUuid = event ^. uuid
      , _addExpertEventDTOPath = toEventPathDTO $ event ^. path
      , _addExpertEventDTOExpertUuid = event ^. expertUuid
      , _addExpertEventDTOName = event ^. name
      , _addExpertEventDTOEmail = event ^. email
      }

instance EventToDTO EditExpertEvent where
  toDTO event =
    EditExpertEventDTO'
      EditExpertEventDTO
      { _editExpertEventDTOUuid = event ^. uuid
      , _editExpertEventDTOPath = toEventPathDTO $ event ^. path
      , _editExpertEventDTOExpertUuid = event ^. expertUuid
      , _editExpertEventDTOName = toEventFieldDTO $ event ^. name
      , _editExpertEventDTOEmail = toEventFieldDTO $ event ^. email
      }

instance EventToDTO DeleteExpertEvent where
  toDTO event =
    DeleteExpertEventDTO'
      DeleteExpertEventDTO
      { _deleteExpertEventDTOUuid = event ^. uuid
      , _deleteExpertEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteExpertEventDTOExpertUuid = event ^. expertUuid
      }

-- -------------------------
-- Reference ---------------
-- -------------------------
instance EventToDTO AddReferenceEvent where
  toDTO event =
    AddReferenceEventDTO'
      AddReferenceEventDTO
      { _addReferenceEventDTOUuid = event ^. uuid
      , _addReferenceEventDTOPath = toEventPathDTO $ event ^. path
      , _addReferenceEventDTOReferenceUuid = event ^. referenceUuid
      , _addReferenceEventDTOChapter = event ^. chapter
      }

instance EventToDTO EditReferenceEvent where
  toDTO event =
    EditReferenceEventDTO'
      EditReferenceEventDTO
      { _editReferenceEventDTOUuid = event ^. uuid
      , _editReferenceEventDTOPath = toEventPathDTO $ event ^. path
      , _editReferenceEventDTOReferenceUuid = event ^. referenceUuid
      , _editReferenceEventDTOChapter = toEventFieldDTO $ event ^. chapter
      }

instance EventToDTO DeleteReferenceEvent where
  toDTO event =
    DeleteReferenceEventDTO'
      DeleteReferenceEventDTO
      { _deleteReferenceEventDTOUuid = event ^. uuid
      , _deleteReferenceEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteReferenceEventDTOReferenceUuid = event ^. referenceUuid
      }
