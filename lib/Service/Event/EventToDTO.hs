module Service.Event.EventToDTO where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.EventPath
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
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
      , _editKnowledgeModelEventDTOChapterUuids = toEventFieldDTO $ event ^. chapterUuids
      , _editKnowledgeModelEventDTOTagUuids = toEventFieldDTO $ event ^. tagUuids
      , _editKnowledgeModelEventDTOIntegrationUuids = toEventFieldDTO $ event ^. integrationUuids
      }

-------------------------
-- Chapter --------------
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
      , _editChapterEventDTOQuestionUuids = toEventFieldDTO $ event ^. questionUuids
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
  toDTO (AddOptionsQuestionEvent' event) =
    AddQuestionEventDTO' $
    AddOptionsQuestionEventDTO' $
    AddOptionsQuestionEventDTO
    { _addOptionsQuestionEventDTOUuid = event ^. uuid
    , _addOptionsQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _addOptionsQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _addOptionsQuestionEventDTOTitle = event ^. title
    , _addOptionsQuestionEventDTOText = event ^. text
    , _addOptionsQuestionEventDTORequiredLevel = event ^. requiredLevel
    , _addOptionsQuestionEventDTOTagUuids = event ^. tagUuids
    }
  toDTO (AddListQuestionEvent' event) =
    AddQuestionEventDTO' $
    AddListQuestionEventDTO' $
    AddListQuestionEventDTO
    { _addListQuestionEventDTOUuid = event ^. uuid
    , _addListQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _addListQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _addListQuestionEventDTOTitle = event ^. title
    , _addListQuestionEventDTOText = event ^. text
    , _addListQuestionEventDTORequiredLevel = event ^. requiredLevel
    , _addListQuestionEventDTOTagUuids = event ^. tagUuids
    , _addListQuestionEventDTOItemTemplateTitle = event ^. itemTemplateTitle
    }
  toDTO (AddValueQuestionEvent' event) =
    AddQuestionEventDTO' $
    AddValueQuestionEventDTO' $
    AddValueQuestionEventDTO
    { _addValueQuestionEventDTOUuid = event ^. uuid
    , _addValueQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _addValueQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _addValueQuestionEventDTOTitle = event ^. title
    , _addValueQuestionEventDTOText = event ^. text
    , _addValueQuestionEventDTORequiredLevel = event ^. requiredLevel
    , _addValueQuestionEventDTOTagUuids = event ^. tagUuids
    , _addValueQuestionEventDTOValueType = event ^. valueType
    }
  toDTO (AddIntegrationQuestionEvent' event) =
    AddQuestionEventDTO' $
    AddIntegrationQuestionEventDTO' $
    AddIntegrationQuestionEventDTO
    { _addIntegrationQuestionEventDTOUuid = event ^. uuid
    , _addIntegrationQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _addIntegrationQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _addIntegrationQuestionEventDTOTitle = event ^. title
    , _addIntegrationQuestionEventDTOText = event ^. text
    , _addIntegrationQuestionEventDTORequiredLevel = event ^. requiredLevel
    , _addIntegrationQuestionEventDTOTagUuids = event ^. tagUuids
    , _addIntegrationQuestionEventDTOIntegrationUuid = event ^. integrationUuid
    , _addIntegrationQuestionEventDTOProps = event ^. props
    }

instance EventToDTO EditQuestionEvent where
  toDTO (EditOptionsQuestionEvent' event) =
    EditQuestionEventDTO' $
    EditOptionsQuestionEventDTO' $
    EditOptionsQuestionEventDTO
    { _editOptionsQuestionEventDTOUuid = event ^. uuid
    , _editOptionsQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _editOptionsQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _editOptionsQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
    , _editOptionsQuestionEventDTOText = toEventFieldDTO $ event ^. text
    , _editOptionsQuestionEventDTORequiredLevel = toEventFieldDTO $ event ^. requiredLevel
    , _editOptionsQuestionEventDTOTagUuids = toEventFieldDTO $ event ^. tagUuids
    , _editOptionsQuestionEventDTOExpertUuids = toEventFieldDTO $ event ^. expertUuids
    , _editOptionsQuestionEventDTOReferenceUuids = toEventFieldDTO $ event ^. referenceUuids
    , _editOptionsQuestionEventDTOAnswerUuids = toEventFieldDTO $ event ^. answerUuids
    }
  toDTO (EditListQuestionEvent' event) =
    EditQuestionEventDTO' $
    EditListQuestionEventDTO' $
    EditListQuestionEventDTO
    { _editListQuestionEventDTOUuid = event ^. uuid
    , _editListQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _editListQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _editListQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
    , _editListQuestionEventDTOText = toEventFieldDTO $ event ^. text
    , _editListQuestionEventDTORequiredLevel = toEventFieldDTO $ event ^. requiredLevel
    , _editListQuestionEventDTOTagUuids = toEventFieldDTO $ event ^. tagUuids
    , _editListQuestionEventDTOExpertUuids = toEventFieldDTO $ event ^. expertUuids
    , _editListQuestionEventDTOReferenceUuids = toEventFieldDTO $ event ^. referenceUuids
    , _editListQuestionEventDTOItemTemplateTitle = toEventFieldDTO $ event ^. itemTemplateTitle
    , _editListQuestionEventDTOItemTemplateQuestionUuids = toEventFieldDTO $ event ^. itemTemplateQuestionUuids
    }
  toDTO (EditValueQuestionEvent' event) =
    EditQuestionEventDTO' $
    EditValueQuestionEventDTO' $
    EditValueQuestionEventDTO
    { _editValueQuestionEventDTOUuid = event ^. uuid
    , _editValueQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _editValueQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _editValueQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
    , _editValueQuestionEventDTOText = toEventFieldDTO $ event ^. text
    , _editValueQuestionEventDTORequiredLevel = toEventFieldDTO $ event ^. requiredLevel
    , _editValueQuestionEventDTOTagUuids = toEventFieldDTO $ event ^. tagUuids
    , _editValueQuestionEventDTOExpertUuids = toEventFieldDTO $ event ^. expertUuids
    , _editValueQuestionEventDTOReferenceUuids = toEventFieldDTO $ event ^. referenceUuids
    , _editValueQuestionEventDTOValueType = toEventFieldDTO $ event ^. valueType
    }
  toDTO (EditIntegrationQuestionEvent' event) =
    EditQuestionEventDTO' $
    EditIntegrationQuestionEventDTO' $
    EditIntegrationQuestionEventDTO
    { _editIntegrationQuestionEventDTOUuid = event ^. uuid
    , _editIntegrationQuestionEventDTOPath = toEventPathDTO $ event ^. path
    , _editIntegrationQuestionEventDTOQuestionUuid = event ^. questionUuid
    , _editIntegrationQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
    , _editIntegrationQuestionEventDTOText = toEventFieldDTO $ event ^. text
    , _editIntegrationQuestionEventDTORequiredLevel = toEventFieldDTO $ event ^. requiredLevel
    , _editIntegrationQuestionEventDTOTagUuids = toEventFieldDTO $ event ^. tagUuids
    , _editIntegrationQuestionEventDTOExpertUuids = toEventFieldDTO $ event ^. expertUuids
    , _editIntegrationQuestionEventDTOReferenceUuids = toEventFieldDTO $ event ^. referenceUuids
    , _editIntegrationQuestionEventDTOIntegrationUuid = toEventFieldDTO $ event ^. integrationUuid
    , _editIntegrationQuestionEventDTOProps = toEventFieldDTO $ event ^. props
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
      , _editAnswerEventDTOFollowUpUuids = toEventFieldDTO $ event ^. followUpUuids
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
  toDTO event =
    DeleteReferenceEventDTO'
      DeleteReferenceEventDTO
      { _deleteReferenceEventDTOUuid = event ^. uuid
      , _deleteReferenceEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteReferenceEventDTOReferenceUuid = event ^. referenceUuid
      }

-------------------------
-- Tag ------------------
-------------------------
instance EventToDTO AddTagEvent where
  toDTO event =
    AddTagEventDTO'
      AddTagEventDTO
      { _addTagEventDTOUuid = event ^. uuid
      , _addTagEventDTOPath = toEventPathDTO $ event ^. path
      , _addTagEventDTOTagUuid = event ^. tagUuid
      , _addTagEventDTOName = event ^. name
      , _addTagEventDTODescription = event ^. description
      , _addTagEventDTOColor = event ^. color
      }

instance EventToDTO EditTagEvent where
  toDTO event =
    EditTagEventDTO'
      EditTagEventDTO
      { _editTagEventDTOUuid = event ^. uuid
      , _editTagEventDTOPath = toEventPathDTO $ event ^. path
      , _editTagEventDTOTagUuid = event ^. tagUuid
      , _editTagEventDTOName = toEventFieldDTO $ event ^. name
      , _editTagEventDTODescription = toEventFieldDTO $ event ^. description
      , _editTagEventDTOColor = toEventFieldDTO $ event ^. color
      }

instance EventToDTO DeleteTagEvent where
  toDTO event =
    DeleteTagEventDTO'
      DeleteTagEventDTO
      { _deleteTagEventDTOUuid = event ^. uuid
      , _deleteTagEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteTagEventDTOTagUuid = event ^. tagUuid
      }

-- -------------------------
-- Integration -------------
-- -------------------------
instance EventToDTO AddIntegrationEvent where
  toDTO event =
    AddIntegrationEventDTO'
      AddIntegrationEventDTO
      { _addIntegrationEventDTOUuid = event ^. uuid
      , _addIntegrationEventDTOPath = toEventPathDTO $ event ^. path
      , _addIntegrationEventDTOIntegrationUuid = event ^. integrationUuid
      , _addIntegrationEventDTOIId = event ^. iId
      , _addIntegrationEventDTOName = event ^. name
      , _addIntegrationEventDTOProps = event ^. props
      , _addIntegrationEventDTOLogo = event ^. logo
      , _addIntegrationEventDTORequestMethod = event ^. requestMethod
      , _addIntegrationEventDTORequestUrl = event ^. requestUrl
      , _addIntegrationEventDTORequestHeaders = event ^. requestHeaders
      , _addIntegrationEventDTORequestBody = event ^. requestBody
      , _addIntegrationEventDTOResponseListField = event ^. responseListField
      , _addIntegrationEventDTOResponseIdField = event ^. responseIdField
      , _addIntegrationEventDTOResponseNameField = event ^. responseNameField
      , _addIntegrationEventDTOItemUrl = event ^. itemUrl
      }

instance EventToDTO EditIntegrationEvent where
  toDTO event =
    EditIntegrationEventDTO'
      EditIntegrationEventDTO
      { _editIntegrationEventDTOUuid = event ^. uuid
      , _editIntegrationEventDTOPath = toEventPathDTO $ event ^. path
      , _editIntegrationEventDTOIntegrationUuid = event ^. integrationUuid
      , _editIntegrationEventDTOIId = toEventFieldDTO $ event ^. iId
      , _editIntegrationEventDTOName = toEventFieldDTO $ event ^. name
      , _editIntegrationEventDTOProps = toEventFieldDTO $ event ^. props
      , _editIntegrationEventDTOLogo = toEventFieldDTO $ event ^. logo
      , _editIntegrationEventDTORequestMethod = toEventFieldDTO $ event ^. requestMethod
      , _editIntegrationEventDTORequestUrl = toEventFieldDTO $ event ^. requestUrl
      , _editIntegrationEventDTORequestHeaders = toEventFieldDTO $ event ^. requestHeaders
      , _editIntegrationEventDTORequestBody = toEventFieldDTO $ event ^. requestBody
      , _editIntegrationEventDTOResponseListField = toEventFieldDTO $ event ^. responseListField
      , _editIntegrationEventDTOResponseIdField = toEventFieldDTO $ event ^. responseIdField
      , _editIntegrationEventDTOResponseNameField = toEventFieldDTO $ event ^. responseNameField
      , _editIntegrationEventDTOItemUrl = toEventFieldDTO $ event ^. itemUrl
      }

instance EventToDTO DeleteIntegrationEvent where
  toDTO event =
    DeleteIntegrationEventDTO'
      DeleteIntegrationEventDTO
      { _deleteIntegrationEventDTOUuid = event ^. uuid
      , _deleteIntegrationEventDTOPath = toEventPathDTO $ event ^. path
      , _deleteIntegrationEventDTOIntegrationUuid = event ^. integrationUuid
      }
