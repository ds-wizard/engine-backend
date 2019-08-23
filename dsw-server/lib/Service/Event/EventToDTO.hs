module Service.Event.EventToDTO where

import Control.Lens ((^.))

import Api.Resource.Event.AnswerEventDTO
import Api.Resource.Event.ChapterEventDTO
import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.ExpertEventDTO
import Api.Resource.Event.IntegrationEventDTO
import Api.Resource.Event.KnowledgeModelEventDTO
import Api.Resource.Event.QuestionEventDTO
import Api.Resource.Event.ReferenceEventDTO
import Api.Resource.Event.TagEventDTO
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
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

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventToDTO AddKnowledgeModelEvent where
  toDTO event =
    AddKnowledgeModelEventDTO'
      AddKnowledgeModelEventDTO
      { _addKnowledgeModelEventDTOUuid = event ^. uuid
      , _addKnowledgeModelEventDTOParentUuid = event ^. parentUuid
      , _addKnowledgeModelEventDTOEntityUuid = event ^. entityUuid
      , _addKnowledgeModelEventDTOName = event ^. name
      }

instance EventToDTO EditKnowledgeModelEvent where
  toDTO event =
    EditKnowledgeModelEventDTO'
      EditKnowledgeModelEventDTO
      { _editKnowledgeModelEventDTOUuid = event ^. uuid
      , _editKnowledgeModelEventDTOParentUuid = event ^. parentUuid
      , _editKnowledgeModelEventDTOEntityUuid = event ^. entityUuid
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
      , _addChapterEventDTOParentUuid = event ^. parentUuid
      , _addChapterEventDTOEntityUuid = event ^. entityUuid
      , _addChapterEventDTOTitle = event ^. title
      , _addChapterEventDTOText = event ^. text
      }

instance EventToDTO EditChapterEvent where
  toDTO event =
    EditChapterEventDTO'
      EditChapterEventDTO
      { _editChapterEventDTOUuid = event ^. uuid
      , _editChapterEventDTOParentUuid = event ^. parentUuid
      , _editChapterEventDTOEntityUuid = event ^. entityUuid
      , _editChapterEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editChapterEventDTOText = toEventFieldDTO $ event ^. text
      , _editChapterEventDTOQuestionUuids = toEventFieldDTO $ event ^. questionUuids
      }

instance EventToDTO DeleteChapterEvent where
  toDTO event =
    DeleteChapterEventDTO'
      DeleteChapterEventDTO
      { _deleteChapterEventDTOUuid = event ^. uuid
      , _deleteChapterEventDTOParentUuid = event ^. parentUuid
      , _deleteChapterEventDTOEntityUuid = event ^. entityUuid
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
    , _addOptionsQuestionEventDTOParentUuid = event ^. parentUuid
    , _addOptionsQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _addListQuestionEventDTOParentUuid = event ^. parentUuid
    , _addListQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _addValueQuestionEventDTOParentUuid = event ^. parentUuid
    , _addValueQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _addIntegrationQuestionEventDTOParentUuid = event ^. parentUuid
    , _addIntegrationQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _editOptionsQuestionEventDTOParentUuid = event ^. parentUuid
    , _editOptionsQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _editListQuestionEventDTOParentUuid = event ^. parentUuid
    , _editListQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _editValueQuestionEventDTOParentUuid = event ^. parentUuid
    , _editValueQuestionEventDTOEntityUuid = event ^. entityUuid
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
    , _editIntegrationQuestionEventDTOParentUuid = event ^. parentUuid
    , _editIntegrationQuestionEventDTOEntityUuid = event ^. entityUuid
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
      , _deleteQuestionEventDTOParentUuid = event ^. parentUuid
      , _deleteQuestionEventDTOEntityUuid = event ^. entityUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventToDTO AddAnswerEvent where
  toDTO event =
    AddAnswerEventDTO'
      AddAnswerEventDTO
      { _addAnswerEventDTOUuid = event ^. uuid
      , _addAnswerEventDTOParentUuid = event ^. parentUuid
      , _addAnswerEventDTOEntityUuid = event ^. entityUuid
      , _addAnswerEventDTOLabel = event ^. label
      , _addAnswerEventDTOAdvice = event ^. advice
      , _addAnswerEventDTOMetricMeasures = toMetricMeasureDTO <$> event ^. metricMeasures
      }

instance EventToDTO EditAnswerEvent where
  toDTO event =
    EditAnswerEventDTO'
      EditAnswerEventDTO
      { _editAnswerEventDTOUuid = event ^. uuid
      , _editAnswerEventDTOParentUuid = event ^. parentUuid
      , _editAnswerEventDTOEntityUuid = event ^. entityUuid
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
      , _deleteAnswerEventDTOParentUuid = event ^. parentUuid
      , _deleteAnswerEventDTOEntityUuid = event ^. entityUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventToDTO AddExpertEvent where
  toDTO event =
    AddExpertEventDTO'
      AddExpertEventDTO
      { _addExpertEventDTOUuid = event ^. uuid
      , _addExpertEventDTOParentUuid = event ^. parentUuid
      , _addExpertEventDTOEntityUuid = event ^. entityUuid
      , _addExpertEventDTOName = event ^. name
      , _addExpertEventDTOEmail = event ^. email
      }

instance EventToDTO EditExpertEvent where
  toDTO event =
    EditExpertEventDTO'
      EditExpertEventDTO
      { _editExpertEventDTOUuid = event ^. uuid
      , _editExpertEventDTOParentUuid = event ^. parentUuid
      , _editExpertEventDTOEntityUuid = event ^. entityUuid
      , _editExpertEventDTOName = toEventFieldDTO $ event ^. name
      , _editExpertEventDTOEmail = toEventFieldDTO $ event ^. email
      }

instance EventToDTO DeleteExpertEvent where
  toDTO event =
    DeleteExpertEventDTO'
      DeleteExpertEventDTO
      { _deleteExpertEventDTOUuid = event ^. uuid
      , _deleteExpertEventDTOParentUuid = event ^. parentUuid
      , _deleteExpertEventDTOEntityUuid = event ^. entityUuid
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
    , _addResourcePageReferenceEventDTOParentUuid = event ^. parentUuid
    , _addResourcePageReferenceEventDTOEntityUuid = event ^. entityUuid
    , _addResourcePageReferenceEventDTOShortUuid = event ^. shortUuid
    }
  toDTO (AddURLReferenceEvent' event) =
    AddReferenceEventDTO' $
    AddURLReferenceEventDTO' $
    AddURLReferenceEventDTO
    { _addURLReferenceEventDTOUuid = event ^. uuid
    , _addURLReferenceEventDTOParentUuid = event ^. parentUuid
    , _addURLReferenceEventDTOEntityUuid = event ^. entityUuid
    , _addURLReferenceEventDTOUrl = event ^. url
    , _addURLReferenceEventDTOLabel = event ^. label
    }
  toDTO (AddCrossReferenceEvent' event) =
    AddReferenceEventDTO' $
    AddCrossReferenceEventDTO' $
    AddCrossReferenceEventDTO
    { _addCrossReferenceEventDTOUuid = event ^. uuid
    , _addCrossReferenceEventDTOParentUuid = event ^. parentUuid
    , _addCrossReferenceEventDTOEntityUuid = event ^. entityUuid
    , _addCrossReferenceEventDTOTargetUuid = event ^. targetUuid
    , _addCrossReferenceEventDTODescription = event ^. description
    }

instance EventToDTO EditReferenceEvent where
  toDTO (EditResourcePageReferenceEvent' event) =
    EditReferenceEventDTO' $
    EditResourcePageReferenceEventDTO' $
    EditResourcePageReferenceEventDTO
    { _editResourcePageReferenceEventDTOUuid = event ^. uuid
    , _editResourcePageReferenceEventDTOParentUuid = event ^. parentUuid
    , _editResourcePageReferenceEventDTOEntityUuid = event ^. entityUuid
    , _editResourcePageReferenceEventDTOShortUuid = toEventFieldDTO $ event ^. shortUuid
    }
  toDTO (EditURLReferenceEvent' event) =
    EditReferenceEventDTO' $
    EditURLReferenceEventDTO' $
    EditURLReferenceEventDTO
    { _editURLReferenceEventDTOUuid = event ^. uuid
    , _editURLReferenceEventDTOParentUuid = event ^. parentUuid
    , _editURLReferenceEventDTOEntityUuid = event ^. entityUuid
    , _editURLReferenceEventDTOUrl = toEventFieldDTO $ event ^. url
    , _editURLReferenceEventDTOLabel = toEventFieldDTO $ event ^. label
    }
  toDTO (EditCrossReferenceEvent' event) =
    EditReferenceEventDTO' $
    EditCrossReferenceEventDTO' $
    EditCrossReferenceEventDTO
    { _editCrossReferenceEventDTOUuid = event ^. uuid
    , _editCrossReferenceEventDTOParentUuid = event ^. parentUuid
    , _editCrossReferenceEventDTOEntityUuid = event ^. entityUuid
    , _editCrossReferenceEventDTOTargetUuid = toEventFieldDTO $ event ^. targetUuid
    , _editCrossReferenceEventDTODescription = toEventFieldDTO $ event ^. description
    }

instance EventToDTO DeleteReferenceEvent where
  toDTO event =
    DeleteReferenceEventDTO'
      DeleteReferenceEventDTO
      { _deleteReferenceEventDTOUuid = event ^. uuid
      , _deleteReferenceEventDTOParentUuid = event ^. parentUuid
      , _deleteReferenceEventDTOEntityUuid = event ^. entityUuid
      }

-------------------------
-- Tag ------------------
-------------------------
instance EventToDTO AddTagEvent where
  toDTO event =
    AddTagEventDTO'
      AddTagEventDTO
      { _addTagEventDTOUuid = event ^. uuid
      , _addTagEventDTOParentUuid = event ^. parentUuid
      , _addTagEventDTOEntityUuid = event ^. entityUuid
      , _addTagEventDTOName = event ^. name
      , _addTagEventDTODescription = event ^. description
      , _addTagEventDTOColor = event ^. color
      }

instance EventToDTO EditTagEvent where
  toDTO event =
    EditTagEventDTO'
      EditTagEventDTO
      { _editTagEventDTOUuid = event ^. uuid
      , _editTagEventDTOParentUuid = event ^. parentUuid
      , _editTagEventDTOEntityUuid = event ^. entityUuid
      , _editTagEventDTOName = toEventFieldDTO $ event ^. name
      , _editTagEventDTODescription = toEventFieldDTO $ event ^. description
      , _editTagEventDTOColor = toEventFieldDTO $ event ^. color
      }

instance EventToDTO DeleteTagEvent where
  toDTO event =
    DeleteTagEventDTO'
      DeleteTagEventDTO
      { _deleteTagEventDTOUuid = event ^. uuid
      , _deleteTagEventDTOParentUuid = event ^. parentUuid
      , _deleteTagEventDTOEntityUuid = event ^. entityUuid
      }

-- -------------------------
-- Integration -------------
-- -------------------------
instance EventToDTO AddIntegrationEvent where
  toDTO event =
    AddIntegrationEventDTO'
      AddIntegrationEventDTO
      { _addIntegrationEventDTOUuid = event ^. uuid
      , _addIntegrationEventDTOParentUuid = event ^. parentUuid
      , _addIntegrationEventDTOEntityUuid = event ^. entityUuid
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
      , _editIntegrationEventDTOParentUuid = event ^. parentUuid
      , _editIntegrationEventDTOEntityUuid = event ^. entityUuid
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
      , _deleteIntegrationEventDTOParentUuid = event ^. parentUuid
      , _deleteIntegrationEventDTOEntityUuid = event ^. entityUuid
      }
