module Service.Event.EventFromDTO where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventFieldDTO
import Api.Resource.Event.EventPathDTO
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

fromEventPathItemDTO :: EventPathItemDTO -> EventPathItem
fromEventPathItemDTO dto = EventPathItem {_eventPathItemPType = dto ^. pType, _eventPathItemUuid = dto ^. uuid}

fromEventPathDTO :: EventPathDTO -> EventPath
fromEventPathDTO dto = fromEventPathItemDTO <$> dto

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventFromDTO AddKnowledgeModelEventDTO where
  fromDTO dto =
    AddKnowledgeModelEvent'
      AddKnowledgeModelEvent
      { _addKnowledgeModelEventUuid = dto ^. uuid
      , _addKnowledgeModelEventPath = fromEventPathDTO $ dto ^. path
      , _addKnowledgeModelEventKmUuid = dto ^. kmUuid
      , _addKnowledgeModelEventName = dto ^. name
      }

instance EventFromDTO EditKnowledgeModelEventDTO where
  fromDTO dto =
    EditKnowledgeModelEvent'
      EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid = dto ^. uuid
      , _editKnowledgeModelEventPath = fromEventPathDTO $ dto ^. path
      , _editKnowledgeModelEventKmUuid = dto ^. kmUuid
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
      , _addChapterEventPath = fromEventPathDTO $ dto ^. path
      , _addChapterEventChapterUuid = dto ^. chapterUuid
      , _addChapterEventTitle = dto ^. title
      , _addChapterEventText = dto ^. text
      }

instance EventFromDTO EditChapterEventDTO where
  fromDTO dto =
    EditChapterEvent'
      EditChapterEvent
      { _editChapterEventUuid = dto ^. uuid
      , _editChapterEventPath = fromEventPathDTO $ dto ^. path
      , _editChapterEventChapterUuid = dto ^. chapterUuid
      , _editChapterEventTitle = fromEventFieldDTO $ dto ^. title
      , _editChapterEventText = fromEventFieldDTO $ dto ^. text
      , _editChapterEventQuestionUuids = fromEventFieldDTO $ dto ^. questionUuids
      }

instance EventFromDTO DeleteChapterEventDTO where
  fromDTO dto =
    DeleteChapterEvent'
      DeleteChapterEvent
      { _deleteChapterEventUuid = dto ^. uuid
      , _deleteChapterEventPath = fromEventPathDTO $ dto ^. path
      , _deleteChapterEventChapterUuid = dto ^. chapterUuid
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
    , _addOptionsQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _addOptionsQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _addListQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _addListQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _addValueQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _addValueQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _addIntegrationQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _addIntegrationQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _editOptionsQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _editOptionsQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _editListQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _editListQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _editValueQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _editValueQuestionEventQuestionUuid = dto ^. questionUuid
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
    , _editIntegrationQuestionEventPath = fromEventPathDTO $ dto ^. path
    , _editIntegrationQuestionEventQuestionUuid = dto ^. questionUuid
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
      , _deleteQuestionEventPath = fromEventPathDTO $ dto ^. path
      , _deleteQuestionEventQuestionUuid = dto ^. questionUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventFromDTO AddAnswerEventDTO where
  fromDTO dto =
    AddAnswerEvent'
      AddAnswerEvent
      { _addAnswerEventUuid = dto ^. uuid
      , _addAnswerEventPath = fromEventPathDTO $ dto ^. path
      , _addAnswerEventAnswerUuid = dto ^. answerUuid
      , _addAnswerEventLabel = dto ^. label
      , _addAnswerEventAdvice = dto ^. advice
      , _addAnswerEventMetricMeasures = fromMetricMeasureDTO <$> dto ^. metricMeasures
      }

instance EventFromDTO EditAnswerEventDTO where
  fromDTO dto =
    EditAnswerEvent'
      EditAnswerEvent
      { _editAnswerEventUuid = dto ^. uuid
      , _editAnswerEventPath = fromEventPathDTO $ dto ^. path
      , _editAnswerEventAnswerUuid = dto ^. answerUuid
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
      , _deleteAnswerEventPath = fromEventPathDTO $ dto ^. path
      , _deleteAnswerEventAnswerUuid = dto ^. answerUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventFromDTO AddExpertEventDTO where
  fromDTO dto =
    AddExpertEvent'
      AddExpertEvent
      { _addExpertEventUuid = dto ^. uuid
      , _addExpertEventPath = fromEventPathDTO $ dto ^. path
      , _addExpertEventExpertUuid = dto ^. expertUuid
      , _addExpertEventName = dto ^. name
      , _addExpertEventEmail = dto ^. email
      }

instance EventFromDTO EditExpertEventDTO where
  fromDTO dto =
    EditExpertEvent'
      EditExpertEvent
      { _editExpertEventUuid = dto ^. uuid
      , _editExpertEventPath = fromEventPathDTO $ dto ^. path
      , _editExpertEventExpertUuid = dto ^. expertUuid
      , _editExpertEventName = fromEventFieldDTO $ dto ^. name
      , _editExpertEventEmail = fromEventFieldDTO $ dto ^. email
      }

instance EventFromDTO DeleteExpertEventDTO where
  fromDTO dto =
    DeleteExpertEvent'
      DeleteExpertEvent
      { _deleteExpertEventUuid = dto ^. uuid
      , _deleteExpertEventPath = fromEventPathDTO $ dto ^. path
      , _deleteExpertEventExpertUuid = dto ^. expertUuid
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
    , _addResourcePageReferenceEventPath = fromEventPathDTO $ event ^. path
    , _addResourcePageReferenceEventReferenceUuid = event ^. referenceUuid
    , _addResourcePageReferenceEventShortUuid = event ^. shortUuid
    }
  fromDTO (AddURLReferenceEventDTO' event) =
    AddReferenceEvent' $
    AddURLReferenceEvent' $
    AddURLReferenceEvent
    { _addURLReferenceEventUuid = event ^. uuid
    , _addURLReferenceEventPath = fromEventPathDTO $ event ^. path
    , _addURLReferenceEventReferenceUuid = event ^. referenceUuid
    , _addURLReferenceEventUrl = event ^. url
    , _addURLReferenceEventLabel = event ^. label
    }
  fromDTO (AddCrossReferenceEventDTO' event) =
    AddReferenceEvent' $
    AddCrossReferenceEvent' $
    AddCrossReferenceEvent
    { _addCrossReferenceEventUuid = event ^. uuid
    , _addCrossReferenceEventPath = fromEventPathDTO $ event ^. path
    , _addCrossReferenceEventReferenceUuid = event ^. referenceUuid
    , _addCrossReferenceEventTargetUuid = event ^. targetUuid
    , _addCrossReferenceEventDescription = event ^. description
    }

instance EventFromDTO EditReferenceEventDTO where
  fromDTO (EditResourcePageReferenceEventDTO' event) =
    EditReferenceEvent' $
    EditResourcePageReferenceEvent' $
    EditResourcePageReferenceEvent
    { _editResourcePageReferenceEventUuid = event ^. uuid
    , _editResourcePageReferenceEventPath = fromEventPathDTO $ event ^. path
    , _editResourcePageReferenceEventReferenceUuid = event ^. referenceUuid
    , _editResourcePageReferenceEventShortUuid = fromEventFieldDTO $ event ^. shortUuid
    }
  fromDTO (EditURLReferenceEventDTO' event) =
    EditReferenceEvent' $
    EditURLReferenceEvent' $
    EditURLReferenceEvent
    { _editURLReferenceEventUuid = event ^. uuid
    , _editURLReferenceEventPath = fromEventPathDTO $ event ^. path
    , _editURLReferenceEventReferenceUuid = event ^. referenceUuid
    , _editURLReferenceEventUrl = fromEventFieldDTO $ event ^. url
    , _editURLReferenceEventLabel = fromEventFieldDTO $ event ^. label
    }
  fromDTO (EditCrossReferenceEventDTO' event) =
    EditReferenceEvent' $
    EditCrossReferenceEvent' $
    EditCrossReferenceEvent
    { _editCrossReferenceEventUuid = event ^. uuid
    , _editCrossReferenceEventPath = fromEventPathDTO $ event ^. path
    , _editCrossReferenceEventReferenceUuid = event ^. referenceUuid
    , _editCrossReferenceEventTargetUuid = fromEventFieldDTO $ event ^. targetUuid
    , _editCrossReferenceEventDescription = fromEventFieldDTO $ event ^. description
    }

instance EventFromDTO DeleteReferenceEventDTO where
  fromDTO event =
    DeleteReferenceEvent'
      DeleteReferenceEvent
      { _deleteReferenceEventUuid = event ^. uuid
      , _deleteReferenceEventPath = fromEventPathDTO $ event ^. path
      , _deleteReferenceEventReferenceUuid = event ^. referenceUuid
      }

-- -------------------------
-- Tag -----------------
-- -------------------------
instance EventFromDTO AddTagEventDTO where
  fromDTO dto =
    AddTagEvent'
      AddTagEvent
      { _addTagEventUuid = dto ^. uuid
      , _addTagEventPath = fromEventPathDTO $ dto ^. path
      , _addTagEventTagUuid = dto ^. tagUuid
      , _addTagEventName = dto ^. name
      , _addTagEventDescription = dto ^. description
      , _addTagEventColor = dto ^. color
      }

instance EventFromDTO EditTagEventDTO where
  fromDTO dto =
    EditTagEvent'
      EditTagEvent
      { _editTagEventUuid = dto ^. uuid
      , _editTagEventPath = fromEventPathDTO $ dto ^. path
      , _editTagEventTagUuid = dto ^. tagUuid
      , _editTagEventName = fromEventFieldDTO $ dto ^. name
      , _editTagEventDescription = fromEventFieldDTO $ dto ^. description
      , _editTagEventColor = fromEventFieldDTO $ dto ^. color
      }

instance EventFromDTO DeleteTagEventDTO where
  fromDTO dto =
    DeleteTagEvent'
      DeleteTagEvent
      { _deleteTagEventUuid = dto ^. uuid
      , _deleteTagEventPath = fromEventPathDTO $ dto ^. path
      , _deleteTagEventTagUuid = dto ^. tagUuid
      }

-- -------------------------
-- Integration -------------
-- -------------------------
instance EventFromDTO AddIntegrationEventDTO where
  fromDTO dto =
    AddIntegrationEvent'
      AddIntegrationEvent
      { _addIntegrationEventUuid = dto ^. uuid
      , _addIntegrationEventPath = fromEventPathDTO $ dto ^. path
      , _addIntegrationEventIntegrationUuid = dto ^. integrationUuid
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
      , _editIntegrationEventPath = fromEventPathDTO $ dto ^. path
      , _editIntegrationEventIntegrationUuid = dto ^. integrationUuid
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
      , _deleteIntegrationEventPath = fromEventPathDTO $ dto ^. path
      , _deleteIntegrationEventIntegrationUuid = dto ^. integrationUuid
      }
