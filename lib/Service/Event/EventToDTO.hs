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
      , _addQuestionEventDTOQType = event ^. qType
      , _addQuestionEventDTOTitle = event ^. title
      , _addQuestionEventDTOText = event ^. text
      , _addQuestionEventDTORequiredLevel = event ^. requiredLevel
      , _addQuestionEventDTOAnswerItemTemplatePlain = toAnswerItemTemplatePlainDTO <$> event ^. answerItemTemplatePlain
      }

instance EventToDTO EditQuestionEvent where
  toDTO event =
    EditQuestionEventDTO'
      EditQuestionEventDTO
      { _editQuestionEventDTOUuid = event ^. uuid
      , _editQuestionEventDTOPath = toEventPathDTO $ event ^. path
      , _editQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _editQuestionEventDTOQType = toEventFieldDTO $ event ^. qType
      , _editQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editQuestionEventDTOText = toEventFieldDTO $ event ^. text
      , _editQuestionEventDTORequiredLevel = toEventFieldDTO $ event ^. requiredLevel
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
      , _addAnswerEventDTOMetricMeasures = toMetricMeasureDTO <$> event ^. metricMeasures
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
      , _editAnswerEventDTOMetricMeasures =
          case event ^. metricMeasures of
            ChangedValue mms -> ChangedValueDTO $ toMetricMeasureDTO <$> mms
            NothingChanged -> NothingChangedDTO
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
  toDTO (AddResourcePageReferenceEvent' event) =
    AddReferenceEventDTO' $
    AddResourcePageReferenceEventDTO' $
    AddResourcePageReferenceEventDTO
    { _addResourcePageReferenceEventDTOUuid = event ^. uuid
    , _addResourcePageReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _addResourcePageReferenceEventDTOReferenceUuid = event ^. referenceUuid
    , _addResourcePageReferenceEventDTOShortUuid = event ^. shortUuid
    }
  toDTO (AddURLReferenceEvent' event) =
    AddReferenceEventDTO' $
    AddURLReferenceEventDTO' $
    AddURLReferenceEventDTO
    { _addURLReferenceEventDTOUuid = event ^. uuid
    , _addURLReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _addURLReferenceEventDTOReferenceUuid = event ^. referenceUuid
    , _addURLReferenceEventDTOUrl = event ^. url
    , _addURLReferenceEventDTOLabel = event ^. label
    }
  toDTO (AddCrossReferenceEvent' event) =
    AddReferenceEventDTO' $
    AddCrossReferenceEventDTO' $
    AddCrossReferenceEventDTO
    { _addCrossReferenceEventDTOUuid = event ^. uuid
    , _addCrossReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _addCrossReferenceEventDTOReferenceUuid = event ^. referenceUuid
    , _addCrossReferenceEventDTOTargetUuid = event ^. targetUuid
    , _addCrossReferenceEventDTODescription = event ^. description
    }

instance EventToDTO EditReferenceEvent where
  toDTO (EditResourcePageReferenceEvent' event) =
    EditReferenceEventDTO' $
    EditResourcePageReferenceEventDTO' $
    EditResourcePageReferenceEventDTO
    { _editResourcePageReferenceEventDTOUuid = event ^. uuid
    , _editResourcePageReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _editResourcePageReferenceEventDTOReferenceUuid = event ^. referenceUuid
    , _editResourcePageReferenceEventDTOShortUuid = toEventFieldDTO $ event ^. shortUuid
    }
  toDTO (EditURLReferenceEvent' event) =
    EditReferenceEventDTO' $
    EditURLReferenceEventDTO' $
    EditURLReferenceEventDTO
    { _editURLReferenceEventDTOUuid = event ^. uuid
    , _editURLReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _editURLReferenceEventDTOReferenceUuid = event ^. referenceUuid
    , _editURLReferenceEventDTOUrl = toEventFieldDTO $ event ^. url
    , _editURLReferenceEventDTOLabel = toEventFieldDTO $ event ^. label
    }
  toDTO (EditCrossReferenceEvent' event) =
    EditReferenceEventDTO' $
    EditCrossReferenceEventDTO' $
    EditCrossReferenceEventDTO
    { _editCrossReferenceEventDTOUuid = event ^. uuid
    , _editCrossReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _editCrossReferenceEventDTOReferenceUuid = event ^. referenceUuid
    , _editCrossReferenceEventDTOTargetUuid = toEventFieldDTO $ event ^. targetUuid
    , _editCrossReferenceEventDTODescription = toEventFieldDTO $ event ^. description
    }

instance EventToDTO DeleteReferenceEvent where
  toDTO (DeleteResourcePageReferenceEvent' event) =
    DeleteReferenceEventDTO' $
    DeleteResourcePageReferenceEventDTO' $
    DeleteResourcePageReferenceEventDTO
    { _deleteResourcePageReferenceEventDTOUuid = event ^. uuid
    , _deleteResourcePageReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _deleteResourcePageReferenceEventDTOReferenceUuid = event ^. referenceUuid
    }
  toDTO (DeleteURLReferenceEvent' event) =
    DeleteReferenceEventDTO' $
    DeleteURLReferenceEventDTO' $
    DeleteURLReferenceEventDTO
    { _deleteURLReferenceEventDTOUuid = event ^. uuid
    , _deleteURLReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _deleteURLReferenceEventDTOReferenceUuid = event ^. referenceUuid
    }
  toDTO (DeleteCrossReferenceEvent' event) =
    DeleteReferenceEventDTO' $
    DeleteCrossReferenceEventDTO' $
    DeleteCrossReferenceEventDTO
    { _deleteCrossReferenceEventDTOUuid = event ^. uuid
    , _deleteCrossReferenceEventDTOPath = toEventPathDTO $ event ^. path
    , _deleteCrossReferenceEventDTOReferenceUuid = event ^. referenceUuid
    }
