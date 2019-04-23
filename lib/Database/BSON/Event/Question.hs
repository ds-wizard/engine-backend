module Database.BSON.Event.Question where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.Event.EventPath ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import LensesConfig
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
  toBSON event =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "OptionsQuestion"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeUUIDList (event ^. tagUuids)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    ]

instance FromBSON AddOptionsQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    qTagUuids <- deserializeMaybeUUIDList $ BSON.lookup "tagUuids" doc
    return
      AddOptionsQuestionEvent
      { _addOptionsQuestionEventUuid = qUuid
      , _addOptionsQuestionEventPath = qPath
      , _addOptionsQuestionEventQuestionUuid = qQuestionUuid
      , _addOptionsQuestionEventTitle = qTitle
      , _addOptionsQuestionEventText = qText
      , _addOptionsQuestionEventRequiredLevel = qRequiredLevel
      , _addOptionsQuestionEventTagUuids = qTagUuids
      }

-- ------------------------------------------------
instance ToBSON AddListQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "ListQuestion"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeUUIDList (event ^. tagUuids)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "itemTemplateTitle" BSON.=: (event ^. itemTemplateTitle)
    ]

instance FromBSON AddListQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    qTagUuids <- deserializeMaybeUUIDList $ BSON.lookup "tagUuids" doc
    qItemTemplateTitle <- BSON.lookup "itemTemplateTitle" doc
    return
      AddListQuestionEvent
      { _addListQuestionEventUuid = qUuid
      , _addListQuestionEventPath = qPath
      , _addListQuestionEventQuestionUuid = qQuestionUuid
      , _addListQuestionEventTitle = qTitle
      , _addListQuestionEventText = qText
      , _addListQuestionEventRequiredLevel = qRequiredLevel
      , _addListQuestionEventTagUuids = qTagUuids
      , _addListQuestionEventItemTemplateTitle = qItemTemplateTitle
      }

-- ------------------------------------------------
instance ToBSON AddValueQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "ValueQuestion"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeUUIDList (event ^. tagUuids)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "valueType" BSON.=: serializeQuestionValueType (event ^. valueType)
    ]

instance FromBSON AddValueQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    qTagUuids <- deserializeMaybeUUIDList $ BSON.lookup "tagUuids" doc
    qValueType <- deserializeQuestionValueType $ BSON.lookup "valueType" doc
    return
      AddValueQuestionEvent
      { _addValueQuestionEventUuid = qUuid
      , _addValueQuestionEventPath = qPath
      , _addValueQuestionEventQuestionUuid = qQuestionUuid
      , _addValueQuestionEventTitle = qTitle
      , _addValueQuestionEventText = qText
      , _addValueQuestionEventRequiredLevel = qRequiredLevel
      , _addValueQuestionEventTagUuids = qTagUuids
      , _addValueQuestionEventValueType = qValueType
      }

-- ------------------------------------------------
instance ToBSON AddIntegrationQuestionEvent where
  toBSON AddIntegrationQuestionEvent {..} =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "questionType" BSON.=: "IntegrationQuestion"
    , "uuid" BSON.=: serializeUUID _addIntegrationQuestionEventUuid
    , "path" BSON.=: _addIntegrationQuestionEventPath
    , "questionUuid" BSON.=: serializeUUID _addIntegrationQuestionEventQuestionUuid
    , "title" BSON.=: _addIntegrationQuestionEventTitle
    , "text" BSON.=: _addIntegrationQuestionEventText
    , "requiredLevel" BSON.=: _addIntegrationQuestionEventRequiredLevel
    , "tagUuids" BSON.=: serializeUUIDList _addIntegrationQuestionEventTagUuids
    , "integrationUuid" BSON.=: serializeUUID _addIntegrationQuestionEventIntegrationUuid
    , "props" BSON.=: _addIntegrationQuestionEventProps
    ]

instance FromBSON AddIntegrationQuestionEvent where
  fromBSON doc = do
    _addIntegrationQuestionEventUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    _addIntegrationQuestionEventPath <- BSON.lookup "path" doc
    _addIntegrationQuestionEventQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    _addIntegrationQuestionEventTitle <- BSON.lookup "title" doc
    _addIntegrationQuestionEventText <- BSON.lookup "text" doc
    _addIntegrationQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    _addIntegrationQuestionEventTagUuids <- deserializeMaybeUUIDList $ BSON.lookup "tagUuids" doc
    _addIntegrationQuestionEventIntegrationUuid <- deserializeMaybeUUID $ BSON.lookup "integrationUuid" doc
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
  toBSON event =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "OptionsQuestion"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeEventFieldUUIDList (event ^. tagUuids)
    , "expertUuids" BSON.=: serializeEventFieldUUIDList (event ^. expertUuids)
    , "referenceUuids" BSON.=: serializeEventFieldUUIDList (event ^. referenceUuids)
    , "answerUuids" BSON.=: serializeEventFieldUUIDList (event ^. answerUuids)
    ]

instance FromBSON EditOptionsQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    let qTagUuids = deserializeEventFieldUUIDList $ BSON.lookup "tagUuids" doc
    let qExpertUuids = deserializeEventFieldUUIDList $ BSON.lookup "expertUuids" doc
    let qReferenceUuids = deserializeEventFieldUUIDList $ BSON.lookup "referenceUuids" doc
    let qAnswerUuids = deserializeEventFieldUUIDList $ BSON.lookup "answerUuids" doc
    return
      EditOptionsQuestionEvent
      { _editOptionsQuestionEventUuid = qUuid
      , _editOptionsQuestionEventPath = qPath
      , _editOptionsQuestionEventQuestionUuid = qQuestionUuid
      , _editOptionsQuestionEventTitle = qTitle
      , _editOptionsQuestionEventText = qText
      , _editOptionsQuestionEventRequiredLevel = qRequiredLevel
      , _editOptionsQuestionEventTagUuids = qTagUuids
      , _editOptionsQuestionEventExpertUuids = qExpertUuids
      , _editOptionsQuestionEventReferenceUuids = qReferenceUuids
      , _editOptionsQuestionEventAnswerUuids = qAnswerUuids
      }

-- ------------------------------------------------
instance ToBSON EditListQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "ListQuestion"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeEventFieldUUIDList (event ^. tagUuids)
    , "expertUuids" BSON.=: serializeEventFieldUUIDList (event ^. expertUuids)
    , "referenceUuids" BSON.=: serializeEventFieldUUIDList (event ^. referenceUuids)
    , "itemTemplateTitle" BSON.=: (event ^. itemTemplateTitle)
    , "itemTemplateQuestionUuids" BSON.=: serializeEventFieldUUIDList (event ^. itemTemplateQuestionUuids)
    ]

instance FromBSON EditListQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    let qTagUuids = deserializeEventFieldUUIDList $ BSON.lookup "tagUuids" doc
    let qExpertUuids = deserializeEventFieldUUIDList $ BSON.lookup "expertUuids" doc
    let qReferenceUuids = deserializeEventFieldUUIDList $ BSON.lookup "referenceUuids" doc
    qItemTemplateTitle <- BSON.lookup "itemTemplateTitle" doc
    let qItemTemplateQuestionUuids = deserializeEventFieldUUIDList $ BSON.lookup "itemTemplateQuestionUuids" doc
    return
      EditListQuestionEvent
      { _editListQuestionEventUuid = qUuid
      , _editListQuestionEventPath = qPath
      , _editListQuestionEventQuestionUuid = qQuestionUuid
      , _editListQuestionEventTitle = qTitle
      , _editListQuestionEventText = qText
      , _editListQuestionEventRequiredLevel = qRequiredLevel
      , _editListQuestionEventTagUuids = qTagUuids
      , _editListQuestionEventExpertUuids = qExpertUuids
      , _editListQuestionEventReferenceUuids = qReferenceUuids
      , _editListQuestionEventItemTemplateTitle = qItemTemplateTitle
      , _editListQuestionEventItemTemplateQuestionUuids = qItemTemplateQuestionUuids
      }

-- ------------------------------------------------
instance ToBSON EditValueQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "ValueQuestion"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeEventFieldUUIDList (event ^. tagUuids)
    , "expertUuids" BSON.=: serializeEventFieldUUIDList (event ^. expertUuids)
    , "referenceUuids" BSON.=: serializeEventFieldUUIDList (event ^. referenceUuids)
    , "valueType" BSON.=: event ^. valueType
    ]

instance FromBSON EditValueQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    let qTagUuids = deserializeEventFieldUUIDList $ BSON.lookup "tagUuids" doc
    let qExpertUuids = deserializeEventFieldUUIDList $ BSON.lookup "expertUuids" doc
    let qReferenceUuids = deserializeEventFieldUUIDList $ BSON.lookup "referenceUuids" doc
    qValueType <- BSON.lookup "valueType" doc
    return
      EditValueQuestionEvent
      { _editValueQuestionEventUuid = qUuid
      , _editValueQuestionEventPath = qPath
      , _editValueQuestionEventQuestionUuid = qQuestionUuid
      , _editValueQuestionEventTitle = qTitle
      , _editValueQuestionEventText = qText
      , _editValueQuestionEventRequiredLevel = qRequiredLevel
      , _editValueQuestionEventTagUuids = qTagUuids
      , _editValueQuestionEventExpertUuids = qExpertUuids
      , _editValueQuestionEventReferenceUuids = qReferenceUuids
      , _editValueQuestionEventValueType = qValueType
      }

-- ------------------------------------------------
instance ToBSON EditIntegrationQuestionEvent where
  toBSON EditIntegrationQuestionEvent {..} =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "questionType" BSON.=: "ValueQuestion"
    , "uuid" BSON.=: serializeUUID _editIntegrationQuestionEventUuid
    , "path" BSON.=: _editIntegrationQuestionEventPath
    , "questionUuid" BSON.=: serializeUUID _editIntegrationQuestionEventQuestionUuid
    , "title" BSON.=: _editIntegrationQuestionEventTitle
    , "text" BSON.=: _editIntegrationQuestionEventText
    , "requiredLevel" BSON.=: _editIntegrationQuestionEventRequiredLevel
    , "tagUuids" BSON.=: serializeEventFieldUUIDList _editIntegrationQuestionEventTagUuids
    , "expertUuids" BSON.=: serializeEventFieldUUIDList _editIntegrationQuestionEventExpertUuids
    , "referenceUuids" BSON.=: serializeEventFieldUUIDList _editIntegrationQuestionEventReferenceUuids
    , "integrationUuid" BSON.=: serializeEventFieldUUID _editIntegrationQuestionEventIntegrationUuid
    , "props" BSON.=: _editIntegrationQuestionEventProps
    ]

instance FromBSON EditIntegrationQuestionEvent where
  fromBSON doc = do
    _editIntegrationQuestionEventUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    _editIntegrationQuestionEventPath <- BSON.lookup "path" doc
    _editIntegrationQuestionEventQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    _editIntegrationQuestionEventTitle <- BSON.lookup "title" doc
    _editIntegrationQuestionEventText <- BSON.lookup "text" doc
    _editIntegrationQuestionEventRequiredLevel <- BSON.lookup "requiredLevel" doc
    let _editIntegrationQuestionEventTagUuids = deserializeEventFieldUUIDList $ BSON.lookup "tagUuids" doc
    let _editIntegrationQuestionEventExpertUuids = deserializeEventFieldUUIDList $ BSON.lookup "expertUuids" doc
    let _editIntegrationQuestionEventReferenceUuids = deserializeEventFieldUUIDList $ BSON.lookup "referenceUuids" doc
    _editIntegrationQuestionEventIntegrationUuid <- deserializeMaybeEventFieldUUID $ BSON.lookup "integrationUuid" doc
    _editIntegrationQuestionEventProps <- BSON.lookup "props" doc
    return EditIntegrationQuestionEvent {..}

-- -------------------------
-- DELETE QUESTION EVENT ---
-- -------------------------
instance ToBSON DeleteQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    ]

instance FromBSON DeleteQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    return
      DeleteQuestionEvent
      { _deleteQuestionEventUuid = qUuid
      , _deleteQuestionEventPath = qPath
      , _deleteQuestionEventQuestionUuid = qQuestionUuid
      }
