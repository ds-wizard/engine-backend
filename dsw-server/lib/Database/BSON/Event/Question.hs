module Database.BSON.Event.Question where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Model.Event.Question.QuestionEvent

-- -------------------------
-- ADD QUESTION EVENT ------
-- -------------------------
instance ToBSON AddQuestionEvent where
  toBSON (AddOptionsQuestionEvent' event) = toBSON event
  toBSON (AddListQuestionEvent' event) = toBSON event
  toBSON (AddValueQuestionEvent' event) = toBSON event
  toBSON (AddIntegrationQuestionEvent' event) = toBSON event

instance FromBSON AddQuestionEvent where
  fromBSON doc = do
    questionType <- BSON.lookup "questionType" doc
    case questionType of
      "OptionsQuestion" -> AddOptionsQuestionEvent' <$> (fromBSON doc :: Maybe AddOptionsQuestionEvent)
      "ListQuestion" -> AddListQuestionEvent' <$> (fromBSON doc :: Maybe AddListQuestionEvent)
      "ValueQuestion" -> AddValueQuestionEvent' <$> (fromBSON doc :: Maybe AddValueQuestionEvent)
      "IntegrationQuestion" -> AddIntegrationQuestionEvent' <$> (fromBSON doc :: Maybe AddIntegrationQuestionEvent)

-- ------------------------------------------------
instance ToBSON AddOptionsQuestionEvent where
  toBSON AddOptionsQuestionEvent {..} =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "OptionsQuestion"
    , "uuid" BSON.=: _addOptionsQuestionEventUuid
    , "parentUuid" BSON.=: _addOptionsQuestionEventParentUuid
    , "entityUuid" BSON.=: _addOptionsQuestionEventEntityUuid
    , "title" BSON.=: _addOptionsQuestionEventTitle
    , "text" BSON.=: _addOptionsQuestionEventText
    , "requiredLevel" BSON.=: _addOptionsQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _addOptionsQuestionEventTagUuids
    ]

instance FromBSON AddOptionsQuestionEvent where
  fromBSON doc = do
    _addOptionsQuestionEventUuid <- BSON.lookup "uuid" doc
    _addOptionsQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _addOptionsQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addOptionsQuestionEventTitle <- BSON.lookup "title" doc
    _addOptionsQuestionEventText <- BSON.lookup "text" doc
    _addOptionsQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _addOptionsQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    return AddOptionsQuestionEvent {..}

-- ------------------------------------------------
instance ToBSON AddListQuestionEvent where
  toBSON AddListQuestionEvent {..} =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "ListQuestion"
    , "uuid" BSON.=: _addListQuestionEventUuid
    , "parentUuid" BSON.=: _addListQuestionEventParentUuid
    , "entityUuid" BSON.=: _addListQuestionEventEntityUuid
    , "title" BSON.=: _addListQuestionEventTitle
    , "text" BSON.=: _addListQuestionEventText
    , "requiredLevel" BSON.=: _addListQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _addListQuestionEventTagUuids
    , "itemTemplateTitle" BSON.=: _addListQuestionEventItemTemplateTitle
    ]

instance FromBSON AddListQuestionEvent where
  fromBSON doc = do
    _addListQuestionEventUuid <- BSON.lookup "uuid" doc
    _addListQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _addListQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addListQuestionEventTitle <- BSON.lookup "title" doc
    _addListQuestionEventText <- BSON.lookup "text" doc
    _addListQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _addListQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _addListQuestionEventItemTemplateTitle <- BSON.lookup "itemTemplateTitle" doc
    return AddListQuestionEvent {..}

-- ------------------------------------------------
instance ToBSON AddValueQuestionEvent where
  toBSON AddValueQuestionEvent {..} =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "ValueQuestion"
    , "uuid" BSON.=: _addValueQuestionEventUuid
    , "parentUuid" BSON.=: _addValueQuestionEventParentUuid
    , "entityUuid" BSON.=: _addValueQuestionEventEntityUuid
    , "title" BSON.=: _addValueQuestionEventTitle
    , "text" BSON.=: _addValueQuestionEventText
    , "requiredLevel" BSON.=: _addValueQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _addValueQuestionEventTagUuids
    , "valueType" BSON.=: _addValueQuestionEventValueType
    ]

instance FromBSON AddValueQuestionEvent where
  fromBSON doc = do
    _addValueQuestionEventUuid <- BSON.lookup "uuid" doc
    _addValueQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _addValueQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addValueQuestionEventTitle <- BSON.lookup "title" doc
    _addValueQuestionEventText <- BSON.lookup "text" doc
    _addValueQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _addValueQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _addValueQuestionEventValueType <- BSON.lookup "valueType" doc
    return AddValueQuestionEvent {..}

-- ------------------------------------------------
instance ToBSON AddIntegrationQuestionEvent where
  toBSON AddIntegrationQuestionEvent {..} =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "IntegrationQuestion"
    , "uuid" BSON.=: _addIntegrationQuestionEventUuid
    , "parentUuid" BSON.=: _addIntegrationQuestionEventParentUuid
    , "entityUuid" BSON.=: _addIntegrationQuestionEventEntityUuid
    , "title" BSON.=: _addIntegrationQuestionEventTitle
    , "text" BSON.=: _addIntegrationQuestionEventText
    , "requiredLevel" BSON.=: _addIntegrationQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _addIntegrationQuestionEventTagUuids
    , "integrationUuid" BSON.=: _addIntegrationQuestionEventIntegrationUuid
    , "props" BSON.=: _addIntegrationQuestionEventProps
    ]

instance FromBSON AddIntegrationQuestionEvent where
  fromBSON doc = do
    _addIntegrationQuestionEventUuid <- BSON.lookup "uuid" doc
    _addIntegrationQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _addIntegrationQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _addIntegrationQuestionEventTitle <- BSON.lookup "title" doc
    _addIntegrationQuestionEventText <- BSON.lookup "text" doc
    _addIntegrationQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _addIntegrationQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _addIntegrationQuestionEventIntegrationUuid <- BSON.lookup "integrationUuid" doc
    _addIntegrationQuestionEventProps <- BSON.lookup "props" doc
    return AddIntegrationQuestionEvent {..}

-- -------------------------
-- EDIT QUESTION EVENT -----
-- -------------------------
instance ToBSON EditQuestionEvent where
  toBSON (EditOptionsQuestionEvent' event) = toBSON event
  toBSON (EditListQuestionEvent' event) = toBSON event
  toBSON (EditValueQuestionEvent' event) = toBSON event
  toBSON (EditIntegrationQuestionEvent' event) = toBSON event

instance FromBSON EditQuestionEvent where
  fromBSON doc = do
    questionType <- BSON.lookup "questionType" doc
    case questionType of
      "OptionsQuestion" -> EditOptionsQuestionEvent' <$> (fromBSON doc :: Maybe EditOptionsQuestionEvent)
      "ListQuestion" -> EditListQuestionEvent' <$> (fromBSON doc :: Maybe EditListQuestionEvent)
      "ValueQuestion" -> EditValueQuestionEvent' <$> (fromBSON doc :: Maybe EditValueQuestionEvent)
      "IntegrationQuestion" -> EditIntegrationQuestionEvent' <$> (fromBSON doc :: Maybe EditIntegrationQuestionEvent)

-- ------------------------------------------------
instance ToBSON EditOptionsQuestionEvent where
  toBSON EditOptionsQuestionEvent {..} =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "OptionsQuestion"
    , "uuid" BSON.=: _editOptionsQuestionEventUuid
    , "parentUuid" BSON.=: _editOptionsQuestionEventParentUuid
    , "entityUuid" BSON.=: _editOptionsQuestionEventEntityUuid
    , "title" BSON.=: _editOptionsQuestionEventTitle
    , "text" BSON.=: _editOptionsQuestionEventText
    , "requiredLevel" BSON.=: _editOptionsQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _editOptionsQuestionEventTagUuids
    , "expertUuids" BSON.=: _editOptionsQuestionEventExpertUuids
    , "referenceUuids" BSON.=: _editOptionsQuestionEventReferenceUuids
    , "answerUuids" BSON.=: _editOptionsQuestionEventAnswerUuids
    ]

instance FromBSON EditOptionsQuestionEvent where
  fromBSON doc = do
    _editOptionsQuestionEventUuid <- BSON.lookup "uuid" doc
    _editOptionsQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _editOptionsQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editOptionsQuestionEventTitle <- BSON.lookup "title" doc
    _editOptionsQuestionEventText <- BSON.lookup "text" doc
    _editOptionsQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _editOptionsQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _editOptionsQuestionEventExpertUuids <- BSON.lookup "expertUuids" doc
    _editOptionsQuestionEventReferenceUuids <- BSON.lookup "referenceUuids" doc
    _editOptionsQuestionEventAnswerUuids <- BSON.lookup "answerUuids" doc
    return EditOptionsQuestionEvent {..}

-- ------------------------------------------------
instance ToBSON EditListQuestionEvent where
  toBSON EditListQuestionEvent {..} =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "ListQuestion"
    , "uuid" BSON.=: _editListQuestionEventUuid
    , "parentUuid" BSON.=: _editListQuestionEventParentUuid
    , "entityUuid" BSON.=: _editListQuestionEventEntityUuid
    , "title" BSON.=: _editListQuestionEventTitle
    , "text" BSON.=: _editListQuestionEventText
    , "requiredLevel" BSON.=: _editListQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _editListQuestionEventTagUuids
    , "expertUuids" BSON.=: _editListQuestionEventExpertUuids
    , "referenceUuids" BSON.=: _editListQuestionEventReferenceUuids
    , "itemTemplateTitle" BSON.=: _editListQuestionEventItemTemplateTitle
    , "itemTemplateQuestionUuids" BSON.=: _editListQuestionEventItemTemplateQuestionUuids
    ]

instance FromBSON EditListQuestionEvent where
  fromBSON doc = do
    _editListQuestionEventUuid <- BSON.lookup "uuid" doc
    _editListQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _editListQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editListQuestionEventTitle <- BSON.lookup "title" doc
    _editListQuestionEventText <- BSON.lookup "text" doc
    _editListQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _editListQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _editListQuestionEventExpertUuids <- BSON.lookup "expertUuids" doc
    _editListQuestionEventReferenceUuids <- BSON.lookup "referenceUuids" doc
    _editListQuestionEventItemTemplateTitle <- BSON.lookup "itemTemplateTitle" doc
    _editListQuestionEventItemTemplateQuestionUuids <- BSON.lookup "itemTemplateQuestionUuids" doc
    return EditListQuestionEvent {..}

-- ------------------------------------------------
instance ToBSON EditValueQuestionEvent where
  toBSON EditValueQuestionEvent {..} =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "ValueQuestion"
    , "uuid" BSON.=: _editValueQuestionEventUuid
    , "parentUuid" BSON.=: _editValueQuestionEventParentUuid
    , "entityUuid" BSON.=: _editValueQuestionEventEntityUuid
    , "title" BSON.=: _editValueQuestionEventTitle
    , "text" BSON.=: _editValueQuestionEventText
    , "requiredLevel" BSON.=: _editValueQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _editValueQuestionEventTagUuids
    , "expertUuids" BSON.=: _editValueQuestionEventExpertUuids
    , "referenceUuids" BSON.=: _editValueQuestionEventReferenceUuids
    , "valueType" BSON.=: _editValueQuestionEventValueType
    ]

instance FromBSON EditValueQuestionEvent where
  fromBSON doc = do
    _editValueQuestionEventUuid <- BSON.lookup "uuid" doc
    _editValueQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _editValueQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editValueQuestionEventTitle <- BSON.lookup "title" doc
    _editValueQuestionEventText <- BSON.lookup "text" doc
    _editValueQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _editValueQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _editValueQuestionEventExpertUuids <- BSON.lookup "expertUuids" doc
    _editValueQuestionEventReferenceUuids <- BSON.lookup "referenceUuids" doc
    _editValueQuestionEventValueType <- BSON.lookup "valueType" doc
    return EditValueQuestionEvent {..}

-- ------------------------------------------------
instance ToBSON EditIntegrationQuestionEvent where
  toBSON EditIntegrationQuestionEvent {..} =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "IntegrationQuestion"
    , "uuid" BSON.=: _editIntegrationQuestionEventUuid
    , "parentUuid" BSON.=: _editIntegrationQuestionEventParentUuid
    , "entityUuid" BSON.=: _editIntegrationQuestionEventEntityUuid
    , "title" BSON.=: _editIntegrationQuestionEventTitle
    , "text" BSON.=: _editIntegrationQuestionEventText
    , "requiredLevel" BSON.=: _editIntegrationQuestionEventRequiredLevel
    , "tagUuids" BSON.=: _editIntegrationQuestionEventTagUuids
    , "expertUuids" BSON.=: _editIntegrationQuestionEventExpertUuids
    , "referenceUuids" BSON.=: _editIntegrationQuestionEventReferenceUuids
    , "integrationUuid" BSON.=: _editIntegrationQuestionEventIntegrationUuid
    , "props" BSON.=: _editIntegrationQuestionEventProps
    ]

instance FromBSON EditIntegrationQuestionEvent where
  fromBSON doc = do
    _editIntegrationQuestionEventUuid <- BSON.lookup "uuid" doc
    _editIntegrationQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _editIntegrationQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    _editIntegrationQuestionEventTitle <- BSON.lookup "title" doc
    _editIntegrationQuestionEventText <- BSON.lookup "text" doc
    _editIntegrationQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _editIntegrationQuestionEventTagUuids <- BSON.lookup "tagUuids" doc
    _editIntegrationQuestionEventExpertUuids <- BSON.lookup "expertUuids" doc
    _editIntegrationQuestionEventReferenceUuids <- BSON.lookup "referenceUuids" doc
    _editIntegrationQuestionEventIntegrationUuid <- BSON.lookup "integrationUuid" doc
    _editIntegrationQuestionEventProps <- BSON.lookup "props" doc
    return EditIntegrationQuestionEvent {..}

-- -------------------------
-- DELETE QUESTION EVENT ---
-- -------------------------
instance ToBSON DeleteQuestionEvent where
  toBSON DeleteQuestionEvent {..} =
    [ "eventType" BSON.=: "DeleteQuestionEvent"
    , "uuid" BSON.=: _deleteQuestionEventUuid
    , "parentUuid" BSON.=: _deleteQuestionEventParentUuid
    , "entityUuid" BSON.=: _deleteQuestionEventEntityUuid
    ]

instance FromBSON DeleteQuestionEvent where
  fromBSON doc = do
    _deleteQuestionEventUuid <- BSON.lookup "uuid" doc
    _deleteQuestionEventParentUuid <- BSON.lookup "parentUuid" doc
    _deleteQuestionEventEntityUuid <- BSON.lookup "entityUuid" doc
    return DeleteQuestionEvent {..}
