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

-- ------------------------
-- ADD QUESTION EVENT -----
-- ------------------------
instance ToBSON AddQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "qType" BSON.=: serializeQuestionType (event ^. qType)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeUUIDList (event ^. tagUuids)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "answerItemTemplatePlain" BSON.=: (event ^. answerItemTemplatePlain)
    ]

instance FromBSON AddQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qQType <- deserializeQuestionType $ BSON.lookup "qType" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    qTagUuids <- deserializeMaybeUUIDList $ BSON.lookup "tagUuids" doc
    qAnswerItemTemplatePlain <- BSON.lookup "answerItemTemplatePlain" doc
    return
      AddQuestionEvent
      { _addQuestionEventUuid = qUuid
      , _addQuestionEventPath = qPath
      , _addQuestionEventQuestionUuid = qQuestionUuid
      , _addQuestionEventQType = qQType
      , _addQuestionEventTitle = qTitle
      , _addQuestionEventText = qText
      , _addQuestionEventRequiredLevel = qRequiredLevel
      , _addQuestionEventTagUuids = qTagUuids
      , _addQuestionEventAnswerItemTemplatePlain = qAnswerItemTemplatePlain
      }

-- ------------------------
-- EDIT QUESTION EVENT ----
-- ------------------------
instance ToBSON EditQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "qType" BSON.=: (event ^. qType)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "requiredLevel" BSON.=: (event ^. requiredLevel)
    , "tagUuids" BSON.=: serializeEventFieldUUIDList (event ^. tagUuids)
    , "answerItemTemplatePlainWithUuids" BSON.=: (event ^. answerItemTemplatePlainWithUuids)
    , "answerUuids" BSON.=: serializeEventFieldMaybeUUIDList (event ^. answerUuids)
    , "expertUuids" BSON.=: serializeEventFieldUUIDList (event ^. expertUuids)
    , "referenceUuids" BSON.=: serializeEventFieldUUIDList (event ^. referenceUuids)
    ]

instance FromBSON EditQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qQType <- BSON.lookup "qType" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qRequiredLevel <- BSON.lookup "requiredLevel" doc
    let qTagUuids = deserializeEventFieldUUIDList $ BSON.lookup "tagUuids" doc
    qAnswerItemTemplatePlainWithUuids <- BSON.lookup "answerItemTemplatePlainWithUuids" doc
    let qAnswerUuids = deserializeEventFieldMaybeUUIDList $ BSON.lookup "answerUuids" doc
    let qExpertUuids = deserializeEventFieldUUIDList $ BSON.lookup "expertUuids" doc
    let qReferenceUuids = deserializeEventFieldUUIDList $ BSON.lookup "referenceUuids" doc
    return
      EditQuestionEvent
      { _editQuestionEventUuid = qUuid
      , _editQuestionEventPath = qPath
      , _editQuestionEventQuestionUuid = qQuestionUuid
      , _editQuestionEventQType = qQType
      , _editQuestionEventTitle = qTitle
      , _editQuestionEventText = qText
      , _editQuestionEventRequiredLevel = qRequiredLevel
      , _editQuestionEventTagUuids = qTagUuids
      , _editQuestionEventAnswerItemTemplatePlainWithUuids = qAnswerItemTemplatePlainWithUuids
      , _editQuestionEventAnswerUuids = qAnswerUuids
      , _editQuestionEventExpertUuids = qExpertUuids
      , _editQuestionEventReferenceUuids = qReferenceUuids
      }

-- ------------------------
-- DELETE QUESTION EVENT --
-- ------------------------
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
