module Database.BSON.Event.FollowUpQuestion where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import LensesConfig
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent

-- ----------------------------------
-- ADD FOLLOW-UP QUESTION EVENT -----
-- ----------------------------------
instance ToBSON AddFollowUpQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddFollowUpQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "shortQuestionUuid" BSON.=: (event ^. shortQuestionUuid)
    , "qType" BSON.=: show (event ^. qType)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    ]

instance FromBSON AddFollowUpQuestionEvent where
  fromBSON doc = do
    fuqUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    fuqKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    fuqChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    fuqAnswerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    fuqQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    fuqShortQuestionUuid <- BSON.lookup "shortQuestionUuid" doc
    fuqQType <- deserializeQuestionType $ BSON.lookup "qType" doc
    fuqTitle <- BSON.lookup "title" doc
    fuqText <- BSON.lookup "text" doc
    return
      AddFollowUpQuestionEvent
      { _addFollowUpQuestionEventUuid = fuqUuid
      , _addFollowUpQuestionEventKmUuid = fuqKmUuid
      , _addFollowUpQuestionEventChapterUuid = fuqChapterUuid
      , _addFollowUpQuestionEventAnswerUuid = fuqAnswerUuid
      , _addFollowUpQuestionEventQuestionUuid = fuqQuestionUuid
      , _addFollowUpQuestionEventShortQuestionUuid = fuqShortQuestionUuid
      , _addFollowUpQuestionEventQType = fuqQType
      , _addFollowUpQuestionEventTitle = fuqTitle
      , _addFollowUpQuestionEventText = fuqText
      }

-- ----------------------------------
-- EDIT FOLLOW-UP QUESTION EVENT ----
-- ----------------------------------
instance ToBSON EditFollowUpQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditFollowUpQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "shortQuestionUuid" BSON.=: (event ^. shortQuestionUuid)
    , "qType" BSON.=: show (event ^. qType)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "answerIds" BSON.=: serializeMaybeUUIDList (event ^. answerIds)
    , "expertIds" BSON.=: serializeMaybeUUIDList (event ^. expertIds)
    , "referenceIds" BSON.=: serializeMaybeUUIDList (event ^. referenceIds)
    ]

instance FromBSON EditFollowUpQuestionEvent where
  fromBSON doc = do
    fuqUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    fuqKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    fuqChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    fuqAnswerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    fuqQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    fuqShortQuestionUuid <- BSON.lookup "shortQuestionUuid" doc
    fuqQType <- deserializeQuestionType <$> BSON.lookup "qType" doc
    fuqTitle <- BSON.lookup "title" doc
    fuqText <- BSON.lookup "text" doc
    let fuqAnswerIds = deserializeMaybeUUIDList $ BSON.lookup "answerIds" doc
    let fuqExpertIds = deserializeMaybeUUIDList $ BSON.lookup "expertIds" doc
    let fuqReferenceIds = deserializeMaybeUUIDList $ BSON.lookup "referenceIds" doc
    return
      EditFollowUpQuestionEvent
      { _editFollowUpQuestionEventUuid = fuqUuid
      , _editFollowUpQuestionEventKmUuid = fuqKmUuid
      , _editFollowUpQuestionEventChapterUuid = fuqChapterUuid
      , _editFollowUpQuestionEventAnswerUuid = fuqAnswerUuid
      , _editFollowUpQuestionEventQuestionUuid = fuqQuestionUuid
      , _editFollowUpQuestionEventShortQuestionUuid = fuqShortQuestionUuid
      , _editFollowUpQuestionEventQType = fuqQType
      , _editFollowUpQuestionEventTitle = fuqTitle
      , _editFollowUpQuestionEventText = fuqText
      , _editFollowUpQuestionEventAnswerIds = fuqAnswerIds
      , _editFollowUpQuestionEventExpertIds = fuqExpertIds
      , _editFollowUpQuestionEventReferenceIds = fuqReferenceIds
      }

-- ----------------------------------
-- DELETE FOLLOW-UP QUESTION EVENT --
-- ----------------------------------
instance ToBSON DeleteFollowUpQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteFollowUpQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. answerUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    ]

instance FromBSON DeleteFollowUpQuestionEvent where
  fromBSON doc = do
    fuqUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    fuqKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    fuqChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    fuqAnswerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    fuqQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    return
      DeleteFollowUpQuestionEvent
      { _deleteFollowUpQuestionEventUuid = fuqUuid
      , _deleteFollowUpQuestionEventKmUuid = fuqKmUuid
      , _deleteFollowUpQuestionEventChapterUuid = fuqChapterUuid
      , _deleteFollowUpQuestionEventAnswerUuid = fuqAnswerUuid
      , _deleteFollowUpQuestionEventQuestionUuid = fuqQuestionUuid
      }
