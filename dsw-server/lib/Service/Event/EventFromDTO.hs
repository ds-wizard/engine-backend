module Service.Event.EventFromDTO where

import Control.Lens ((^.))

import Api.Resource.Event.AnswerEventDTO
import Api.Resource.Event.ChapterEventDTO
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
import Model.Event.Event
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
class EventFromDTO a where
  fromDTO :: a -> Event

fromEventFieldDTO :: EventFieldDTO a -> EventField a
fromEventFieldDTO (ChangedValueDTO value) = ChangedValue value
fromEventFieldDTO NothingChangedDTO = NothingChanged

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventFromDTO AddKnowledgeModelEventDTO where
  fromDTO dto =
    AddKnowledgeModelEvent'
      AddKnowledgeModelEvent
      { _addKnowledgeModelEventUuid = dto ^. uuid
      , _addKnowledgeModelEventParentUuid = dto ^. parentUuid
      , _addKnowledgeModelEventEntityUuid = dto ^. entityUuid
      , _addKnowledgeModelEventName = dto ^. name
      }

instance EventFromDTO EditKnowledgeModelEventDTO where
  fromDTO dto =
    EditKnowledgeModelEvent'
      EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid = dto ^. uuid
      , _editKnowledgeModelEventParentUuid = dto ^. parentUuid
      , _editKnowledgeModelEventEntityUuid = dto ^. entityUuid
      , _editKnowledgeModelEventName = fromEventFieldDTO $ dto ^. name
      , _editKnowledgeModelEventChapterUuids = fromEventFieldDTO $ dto ^. chapterUuids
      , _editKnowledgeModelEventTagUuids = fromEventFieldDTO $ dto ^. tagUuids
      , _editKnowledgeModelEventIntegrationUuids = fromEventFieldDTO $ dto ^. integrationUuids
      }

-- -------------------------
-- Chapter -----------------
-- -------------------------
instance EventFromDTO AddChapterEventDTO where
  fromDTO dto =
    AddChapterEvent'
      AddChapterEvent
      { _addChapterEventUuid = dto ^. uuid
      , _addChapterEventParentUuid = dto ^. parentUuid
      , _addChapterEventEntityUuid = dto ^. entityUuid
      , _addChapterEventTitle = dto ^. title
      , _addChapterEventText = dto ^. text
      }

instance EventFromDTO EditChapterEventDTO where
  fromDTO dto =
    EditChapterEvent'
      EditChapterEvent
      { _editChapterEventUuid = dto ^. uuid
      , _editChapterEventParentUuid = dto ^. parentUuid
      , _editChapterEventEntityUuid = dto ^. entityUuid
      , _editChapterEventTitle = fromEventFieldDTO $ dto ^. title
      , _editChapterEventText = fromEventFieldDTO $ dto ^. text
      , _editChapterEventQuestionUuids = fromEventFieldDTO $ dto ^. questionUuids
      }

instance EventFromDTO DeleteChapterEventDTO where
  fromDTO dto =
    DeleteChapterEvent'
      DeleteChapterEvent
      { _deleteChapterEventUuid = dto ^. uuid
      , _deleteChapterEventParentUuid = dto ^. parentUuid
      , _deleteChapterEventEntityUuid = dto ^. entityUuid
      }

-- -------------------------
-- Question ----------------
-- -------------------------
instance EventFromDTO AddQuestionEventDTO where
  fromDTO (AddOptionsQuestionEventDTO' dto) =
    AddQuestionEvent' $
    AddOptionsQuestionEvent' $
    AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = dto ^. uuid
    , _addOptionsQuestionEventParentUuid = dto ^. parentUuid
    , _addOptionsQuestionEventEntityUuid = dto ^. entityUuid
    , _addOptionsQuestionEventTitle = dto ^. title
    , _addOptionsQuestionEventText = dto ^. text
    , _addOptionsQuestionEventRequiredLevel = dto ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = dto ^. tagUuids
    }
  fromDTO (AddListQuestionEventDTO' dto) =
    AddQuestionEvent' $
    AddListQuestionEvent' $
    AddListQuestionEvent
    { _addListQuestionEventUuid = dto ^. uuid
    , _addListQuestionEventParentUuid = dto ^. parentUuid
    , _addListQuestionEventEntityUuid = dto ^. entityUuid
    , _addListQuestionEventTitle = dto ^. title
    , _addListQuestionEventText = dto ^. text
    , _addListQuestionEventRequiredLevel = dto ^. requiredLevel
    , _addListQuestionEventTagUuids = dto ^. tagUuids
    , _addListQuestionEventItemTemplateTitle = dto ^. itemTemplateTitle
    }
  fromDTO (AddValueQuestionEventDTO' dto) =
    AddQuestionEvent' $
    AddValueQuestionEvent' $
    AddValueQuestionEvent
    { _addValueQuestionEventUuid = dto ^. uuid
    , _addValueQuestionEventParentUuid = dto ^. parentUuid
    , _addValueQuestionEventEntityUuid = dto ^. entityUuid
    , _addValueQuestionEventTitle = dto ^. title
    , _addValueQuestionEventText = dto ^. text
    , _addValueQuestionEventRequiredLevel = dto ^. requiredLevel
    , _addValueQuestionEventTagUuids = dto ^. tagUuids
    , _addValueQuestionEventValueType = dto ^. valueType
    }
  fromDTO (AddIntegrationQuestionEventDTO' dto) =
    AddQuestionEvent' $
    AddIntegrationQuestionEvent' $
    AddIntegrationQuestionEvent
    { _addIntegrationQuestionEventUuid = dto ^. uuid
    , _addIntegrationQuestionEventParentUuid = dto ^. parentUuid
    , _addIntegrationQuestionEventEntityUuid = dto ^. entityUuid
    , _addIntegrationQuestionEventTitle = dto ^. title
    , _addIntegrationQuestionEventText = dto ^. text
    , _addIntegrationQuestionEventRequiredLevel = dto ^. requiredLevel
    , _addIntegrationQuestionEventTagUuids = dto ^. tagUuids
    , _addIntegrationQuestionEventIntegrationUuid = dto ^. integrationUuid
    , _addIntegrationQuestionEventProps = dto ^. props
    }

instance EventFromDTO EditQuestionEventDTO where
  fromDTO (EditOptionsQuestionEventDTO' dto) =
    EditQuestionEvent' $
    EditOptionsQuestionEvent' $
    EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = dto ^. uuid
    , _editOptionsQuestionEventParentUuid = dto ^. parentUuid
    , _editOptionsQuestionEventEntityUuid = dto ^. entityUuid
    , _editOptionsQuestionEventTitle = fromEventFieldDTO $ dto ^. title
    , _editOptionsQuestionEventText = fromEventFieldDTO $ dto ^. text
    , _editOptionsQuestionEventRequiredLevel = fromEventFieldDTO $ dto ^. requiredLevel
    , _editOptionsQuestionEventTagUuids = fromEventFieldDTO $ dto ^. tagUuids
    , _editOptionsQuestionEventExpertUuids = fromEventFieldDTO $ dto ^. expertUuids
    , _editOptionsQuestionEventReferenceUuids = fromEventFieldDTO $ dto ^. referenceUuids
    , _editOptionsQuestionEventAnswerUuids = fromEventFieldDTO $ dto ^. answerUuids
    }
  fromDTO (EditListQuestionEventDTO' dto) =
    EditQuestionEvent' $
    EditListQuestionEvent' $
    EditListQuestionEvent
    { _editListQuestionEventUuid = dto ^. uuid
    , _editListQuestionEventParentUuid = dto ^. parentUuid
    , _editListQuestionEventEntityUuid = dto ^. entityUuid
    , _editListQuestionEventTitle = fromEventFieldDTO $ dto ^. title
    , _editListQuestionEventText = fromEventFieldDTO $ dto ^. text
    , _editListQuestionEventRequiredLevel = fromEventFieldDTO $ dto ^. requiredLevel
    , _editListQuestionEventTagUuids = fromEventFieldDTO $ dto ^. tagUuids
    , _editListQuestionEventExpertUuids = fromEventFieldDTO $ dto ^. expertUuids
    , _editListQuestionEventReferenceUuids = fromEventFieldDTO $ dto ^. referenceUuids
    , _editListQuestionEventItemTemplateTitle = fromEventFieldDTO $ dto ^. itemTemplateTitle
    , _editListQuestionEventItemTemplateQuestionUuids = fromEventFieldDTO $ dto ^. itemTemplateQuestionUuids
    }
  fromDTO (EditValueQuestionEventDTO' dto) =
    EditQuestionEvent' $
    EditValueQuestionEvent' $
    EditValueQuestionEvent
    { _editValueQuestionEventUuid = dto ^. uuid
    , _editValueQuestionEventParentUuid = dto ^. parentUuid
    , _editValueQuestionEventEntityUuid = dto ^. entityUuid
    , _editValueQuestionEventTitle = fromEventFieldDTO $ dto ^. title
    , _editValueQuestionEventText = fromEventFieldDTO $ dto ^. text
    , _editValueQuestionEventRequiredLevel = fromEventFieldDTO $ dto ^. requiredLevel
    , _editValueQuestionEventTagUuids = fromEventFieldDTO $ dto ^. tagUuids
    , _editValueQuestionEventExpertUuids = fromEventFieldDTO $ dto ^. expertUuids
    , _editValueQuestionEventReferenceUuids = fromEventFieldDTO $ dto ^. referenceUuids
    , _editValueQuestionEventValueType = fromEventFieldDTO $ dto ^. valueType
    }
  fromDTO (EditIntegrationQuestionEventDTO' dto) =
    EditQuestionEvent' $
    EditIntegrationQuestionEvent' $
    EditIntegrationQuestionEvent
    { _editIntegrationQuestionEventUuid = dto ^. uuid
    , _editIntegrationQuestionEventParentUuid = dto ^. parentUuid
    , _editIntegrationQuestionEventEntityUuid = dto ^. entityUuid
    , _editIntegrationQuestionEventTitle = fromEventFieldDTO $ dto ^. title
    , _editIntegrationQuestionEventText = fromEventFieldDTO $ dto ^. text
    , _editIntegrationQuestionEventRequiredLevel = fromEventFieldDTO $ dto ^. requiredLevel
    , _editIntegrationQuestionEventTagUuids = fromEventFieldDTO $ dto ^. tagUuids
    , _editIntegrationQuestionEventExpertUuids = fromEventFieldDTO $ dto ^. expertUuids
    , _editIntegrationQuestionEventReferenceUuids = fromEventFieldDTO $ dto ^. referenceUuids
    , _editIntegrationQuestionEventIntegrationUuid = fromEventFieldDTO $ dto ^. integrationUuid
    , _editIntegrationQuestionEventProps = fromEventFieldDTO $ dto ^. props
    }

instance EventFromDTO DeleteQuestionEventDTO where
  fromDTO dto =
    DeleteQuestionEvent'
      DeleteQuestionEvent
      { _deleteQuestionEventUuid = dto ^. uuid
      , _deleteQuestionEventParentUuid = dto ^. parentUuid
      , _deleteQuestionEventEntityUuid = dto ^. entityUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventFromDTO AddAnswerEventDTO where
  fromDTO dto =
    AddAnswerEvent'
      AddAnswerEvent
      { _addAnswerEventUuid = dto ^. uuid
      , _addAnswerEventParentUuid = dto ^. parentUuid
      , _addAnswerEventEntityUuid = dto ^. entityUuid
      , _addAnswerEventLabel = dto ^. label
      , _addAnswerEventAdvice = dto ^. advice
      , _addAnswerEventMetricMeasures = fromMetricMeasureDTO <$> dto ^. metricMeasures
      }

instance EventFromDTO EditAnswerEventDTO where
  fromDTO dto =
    EditAnswerEvent'
      EditAnswerEvent
      { _editAnswerEventUuid = dto ^. uuid
      , _editAnswerEventParentUuid = dto ^. parentUuid
      , _editAnswerEventEntityUuid = dto ^. entityUuid
      , _editAnswerEventLabel = fromEventFieldDTO $ dto ^. label
      , _editAnswerEventAdvice = fromEventFieldDTO $ dto ^. advice
      , _editAnswerEventFollowUpUuids = fromEventFieldDTO $ dto ^. followUpUuids
      , _editAnswerEventMetricMeasures =
          case dto ^. metricMeasures of
            ChangedValueDTO mms -> ChangedValue $ fromMetricMeasureDTO <$> mms
            NothingChangedDTO -> NothingChanged
      }

instance EventFromDTO DeleteAnswerEventDTO where
  fromDTO dto =
    DeleteAnswerEvent'
      DeleteAnswerEvent
      { _deleteAnswerEventUuid = dto ^. uuid
      , _deleteAnswerEventParentUuid = dto ^. parentUuid
      , _deleteAnswerEventEntityUuid = dto ^. entityUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventFromDTO AddExpertEventDTO where
  fromDTO dto =
    AddExpertEvent'
      AddExpertEvent
      { _addExpertEventUuid = dto ^. uuid
      , _addExpertEventParentUuid = dto ^. parentUuid
      , _addExpertEventEntityUuid = dto ^. entityUuid
      , _addExpertEventName = dto ^. name
      , _addExpertEventEmail = dto ^. email
      }

instance EventFromDTO EditExpertEventDTO where
  fromDTO dto =
    EditExpertEvent'
      EditExpertEvent
      { _editExpertEventUuid = dto ^. uuid
      , _editExpertEventParentUuid = dto ^. parentUuid
      , _editExpertEventEntityUuid = dto ^. entityUuid
      , _editExpertEventName = fromEventFieldDTO $ dto ^. name
      , _editExpertEventEmail = fromEventFieldDTO $ dto ^. email
      }

instance EventFromDTO DeleteExpertEventDTO where
  fromDTO dto =
    DeleteExpertEvent'
      DeleteExpertEvent
      { _deleteExpertEventUuid = dto ^. uuid
      , _deleteExpertEventParentUuid = dto ^. parentUuid
      , _deleteExpertEventEntityUuid = dto ^. entityUuid
      }

-- -------------------------
-- Reference ---------------
-- -------------------------
instance EventFromDTO AddReferenceEventDTO where
  fromDTO (AddResourcePageReferenceEventDTO' event) =
    AddReferenceEvent' $
    AddResourcePageReferenceEvent' $
    AddResourcePageReferenceEvent
    { _addResourcePageReferenceEventUuid = event ^. uuid
    , _addResourcePageReferenceEventParentUuid = event ^. parentUuid
    , _addResourcePageReferenceEventEntityUuid = event ^. entityUuid
    , _addResourcePageReferenceEventShortUuid = event ^. shortUuid
    }
  fromDTO (AddURLReferenceEventDTO' event) =
    AddReferenceEvent' $
    AddURLReferenceEvent' $
    AddURLReferenceEvent
    { _addURLReferenceEventUuid = event ^. uuid
    , _addURLReferenceEventParentUuid = event ^. parentUuid
    , _addURLReferenceEventEntityUuid = event ^. entityUuid
    , _addURLReferenceEventUrl = event ^. url
    , _addURLReferenceEventLabel = event ^. label
    }
  fromDTO (AddCrossReferenceEventDTO' event) =
    AddReferenceEvent' $
    AddCrossReferenceEvent' $
    AddCrossReferenceEvent
    { _addCrossReferenceEventUuid = event ^. uuid
    , _addCrossReferenceEventParentUuid = event ^. parentUuid
    , _addCrossReferenceEventEntityUuid = event ^. entityUuid
    , _addCrossReferenceEventTargetUuid = event ^. targetUuid
    , _addCrossReferenceEventDescription = event ^. description
    }

instance EventFromDTO EditReferenceEventDTO where
  fromDTO (EditResourcePageReferenceEventDTO' event) =
    EditReferenceEvent' $
    EditResourcePageReferenceEvent' $
    EditResourcePageReferenceEvent
    { _editResourcePageReferenceEventUuid = event ^. uuid
    , _editResourcePageReferenceEventParentUuid = event ^. parentUuid
    , _editResourcePageReferenceEventEntityUuid = event ^. entityUuid
    , _editResourcePageReferenceEventShortUuid = fromEventFieldDTO $ event ^. shortUuid
    }
  fromDTO (EditURLReferenceEventDTO' event) =
    EditReferenceEvent' $
    EditURLReferenceEvent' $
    EditURLReferenceEvent
    { _editURLReferenceEventUuid = event ^. uuid
    , _editURLReferenceEventParentUuid = event ^. parentUuid
    , _editURLReferenceEventEntityUuid = event ^. entityUuid
    , _editURLReferenceEventUrl = fromEventFieldDTO $ event ^. url
    , _editURLReferenceEventLabel = fromEventFieldDTO $ event ^. label
    }
  fromDTO (EditCrossReferenceEventDTO' event) =
    EditReferenceEvent' $
    EditCrossReferenceEvent' $
    EditCrossReferenceEvent
    { _editCrossReferenceEventUuid = event ^. uuid
    , _editCrossReferenceEventParentUuid = event ^. parentUuid
    , _editCrossReferenceEventEntityUuid = event ^. entityUuid
    , _editCrossReferenceEventTargetUuid = fromEventFieldDTO $ event ^. targetUuid
    , _editCrossReferenceEventDescription = fromEventFieldDTO $ event ^. description
    }

instance EventFromDTO DeleteReferenceEventDTO where
  fromDTO event =
    DeleteReferenceEvent'
      DeleteReferenceEvent
      { _deleteReferenceEventUuid = event ^. uuid
      , _deleteReferenceEventParentUuid = event ^. parentUuid
      , _deleteReferenceEventEntityUuid = event ^. entityUuid
      }

-- -------------------------
-- Tag -----------------
-- -------------------------
instance EventFromDTO AddTagEventDTO where
  fromDTO dto =
    AddTagEvent'
      AddTagEvent
      { _addTagEventUuid = dto ^. uuid
      , _addTagEventParentUuid = dto ^. parentUuid
      , _addTagEventEntityUuid = dto ^. entityUuid
      , _addTagEventName = dto ^. name
      , _addTagEventDescription = dto ^. description
      , _addTagEventColor = dto ^. color
      }

instance EventFromDTO EditTagEventDTO where
  fromDTO dto =
    EditTagEvent'
      EditTagEvent
      { _editTagEventUuid = dto ^. uuid
      , _editTagEventParentUuid = dto ^. parentUuid
      , _editTagEventEntityUuid = dto ^. entityUuid
      , _editTagEventName = fromEventFieldDTO $ dto ^. name
      , _editTagEventDescription = fromEventFieldDTO $ dto ^. description
      , _editTagEventColor = fromEventFieldDTO $ dto ^. color
      }

instance EventFromDTO DeleteTagEventDTO where
  fromDTO dto =
    DeleteTagEvent'
      DeleteTagEvent
      { _deleteTagEventUuid = dto ^. uuid
      , _deleteTagEventParentUuid = dto ^. parentUuid
      , _deleteTagEventEntityUuid = dto ^. entityUuid
      }

-- -------------------------
-- Integration -------------
-- -------------------------
instance EventFromDTO AddIntegrationEventDTO where
  fromDTO dto =
    AddIntegrationEvent'
      AddIntegrationEvent
      { _addIntegrationEventUuid = dto ^. uuid
      , _addIntegrationEventParentUuid = dto ^. parentUuid
      , _addIntegrationEventEntityUuid = dto ^. entityUuid
      , _addIntegrationEventIId = dto ^. iId
      , _addIntegrationEventName = dto ^. name
      , _addIntegrationEventProps = dto ^. props
      , _addIntegrationEventLogo = dto ^. logo
      , _addIntegrationEventRequestMethod = dto ^. requestMethod
      , _addIntegrationEventRequestUrl = dto ^. requestUrl
      , _addIntegrationEventRequestHeaders = dto ^. requestHeaders
      , _addIntegrationEventRequestBody = dto ^. requestBody
      , _addIntegrationEventResponseListField = dto ^. responseListField
      , _addIntegrationEventResponseIdField = dto ^. responseIdField
      , _addIntegrationEventResponseNameField = dto ^. responseNameField
      , _addIntegrationEventItemUrl = dto ^. itemUrl
      }

instance EventFromDTO EditIntegrationEventDTO where
  fromDTO dto =
    EditIntegrationEvent'
      EditIntegrationEvent
      { _editIntegrationEventUuid = dto ^. uuid
      , _editIntegrationEventParentUuid = dto ^. parentUuid
      , _editIntegrationEventEntityUuid = dto ^. entityUuid
      , _editIntegrationEventIId = fromEventFieldDTO $ dto ^. iId
      , _editIntegrationEventName = fromEventFieldDTO $ dto ^. name
      , _editIntegrationEventProps = fromEventFieldDTO $ dto ^. props
      , _editIntegrationEventLogo = fromEventFieldDTO $ dto ^. logo
      , _editIntegrationEventRequestMethod = fromEventFieldDTO $ dto ^. requestMethod
      , _editIntegrationEventRequestUrl = fromEventFieldDTO $ dto ^. requestUrl
      , _editIntegrationEventRequestHeaders = fromEventFieldDTO $ dto ^. requestHeaders
      , _editIntegrationEventRequestBody = fromEventFieldDTO $ dto ^. requestBody
      , _editIntegrationEventResponseListField = fromEventFieldDTO $ dto ^. responseListField
      , _editIntegrationEventResponseIdField = fromEventFieldDTO $ dto ^. responseIdField
      , _editIntegrationEventResponseNameField = fromEventFieldDTO $ dto ^. responseNameField
      , _editIntegrationEventItemUrl = fromEventFieldDTO $ dto ^. itemUrl
      }

instance EventFromDTO DeleteIntegrationEventDTO where
  fromDTO dto =
    DeleteIntegrationEvent'
      DeleteIntegrationEvent
      { _deleteIntegrationEventUuid = dto ^. uuid
      , _deleteIntegrationEventParentUuid = dto ^. parentUuid
      , _deleteIntegrationEventEntityUuid = dto ^. entityUuid
      }
