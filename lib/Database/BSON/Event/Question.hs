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
    qAnswerItemTemplatePlain <- BSON.lookup "answerItemTemplatePlain" doc
    return
      AddQuestionEvent
      { _addQuestionEventUuid = qUuid
      , _addQuestionEventPath = qPath
      , _addQuestionEventQuestionUuid = qQuestionUuid
      , _addQuestionEventQType = qQType
      , _addQuestionEventTitle = qTitle
      , _addQuestionEventText = qText
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
    , "answerItemTemplatePlainWithIds" BSON.=: (event ^. answerItemTemplatePlainWithIds)
    , "answerIds" BSON.=: serializeEventFieldMaybeUUIDList (event ^. answerIds)
    , "expertIds" BSON.=: serializeEventFieldUUIDList (event ^. expertIds)
    , "referenceIds" BSON.=: serializeEventFieldUUIDList (event ^. referenceIds)
    ]

instance FromBSON EditQuestionEvent where
  fromBSON doc = do
    qUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    qPath <- BSON.lookup "path" doc
    qQuestionUuid <- deserializeMaybeUUID $ BSON.lookup "questionUuid" doc
    qQType <- BSON.lookup "qType" doc
    qTitle <- BSON.lookup "title" doc
    qText <- BSON.lookup "text" doc
    qAnswerItemTemplatePlainWithIds <- BSON.lookup "answerItemTemplatePlainWithIds" doc
    let qAnswerIds = deserializeEventFieldMaybeUUIDList $ BSON.lookup "answerIds" doc
    let qExpertIds = deserializeEventFieldUUIDList $ BSON.lookup "expertIds" doc
    let qReferenceIds = deserializeEventFieldUUIDList $ BSON.lookup "referenceIds" doc
    return
      EditQuestionEvent
      { _editQuestionEventUuid = qUuid
      , _editQuestionEventPath = qPath
      , _editQuestionEventQuestionUuid = qQuestionUuid
      , _editQuestionEventQType = qQType
      , _editQuestionEventTitle = qTitle
      , _editQuestionEventText = qText
      , _editQuestionEventAnswerItemTemplatePlainWithIds = qAnswerItemTemplatePlainWithIds
      , _editQuestionEventAnswerIds = qAnswerIds
      , _editQuestionEventExpertIds = qExpertIds
      , _editQuestionEventReferenceIds = qReferenceIds
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
