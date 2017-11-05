module Database.BSON.Event.Question where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.DeleteQuestionEvent
import Model.Event.Question.EditQuestionEvent

-- ------------------------
-- ADD QUESTION EVENT -----
-- ------------------------
instance ToBSON AddQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. aqUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. aqKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. aqChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. aqQuestionUuid)
    , "shortQuestionUuid" BSON.=: (event ^. aqShortQuestionUuid)
    , "qType" BSON.=: (event ^. aqType)
    , "title" BSON.=: (event ^. aqTitle)
    , "text" BSON.=: (event ^. aqText)
    ]

instance FromBSON AddQuestionEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    shortQuestionUuid <- BSON.lookup "shortQuestionUuid" doc
    qType <- BSON.lookup "qType" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    return
      AddQuestionEvent
      { _aqUuid = uuid
      , _aqKmUuid = kmUuid
      , _aqChapterUuid = chapterUuid
      , _aqQuestionUuid = questionUuid
      , _aqShortQuestionUuid = shortQuestionUuid
      , _aqType = qType
      , _aqTitle = title
      , _aqText = text
      }

-- ------------------------
-- EDIT QUESTION EVENT ----
-- ------------------------
instance ToBSON EditQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. eqUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. eqKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. eqChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. eqQuestionUuid)
    , "shortQuestionUuid" BSON.=: (event ^. eqShortQuestionUuid)
    , "qType" BSON.=: (event ^. eqType)
    , "title" BSON.=: (event ^. eqTitle)
    , "text" BSON.=: (event ^. eqText)
    , "answerIds" BSON.=: serializeMaybeUUIDList (event ^. eqAnswerIds)
    , "expertIds" BSON.=: serializeMaybeUUIDList (event ^. eqExpertIds)
    , "referenceIds" BSON.=: serializeMaybeUUIDList (event ^. eqReferenceIds)
    ]

instance FromBSON EditQuestionEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    shortQuestionUuid <- BSON.lookup "shortQuestionUuid" doc
    qType <- BSON.lookup "qType" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    let answerIds = deserializeMaybeUUIDList $ BSON.lookup "answerUuids" doc
    let expertIds = deserializeMaybeUUIDList $ BSON.lookup "expertUuids" doc
    let referenceIds =
          deserializeMaybeUUIDList $ BSON.lookup "referenceUuids" doc
    return
      EditQuestionEvent
      { _eqUuid = uuid
      , _eqKmUuid = kmUuid
      , _eqChapterUuid = chapterUuid
      , _eqQuestionUuid = questionUuid
      , _eqShortQuestionUuid = shortQuestionUuid
      , _eqType = qType
      , _eqTitle = title
      , _eqText = text
      , _eqAnswerIds = answerIds
      , _eqExpertIds = expertIds
      , _eqReferenceIds = referenceIds
      }

-- ------------------------
-- DELETE QUESTION EVENT --
-- ------------------------
instance ToBSON DeleteQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. dqUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. dqKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. dqChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. dqQuestionUuid)
    ]

instance FromBSON DeleteQuestionEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    return
      DeleteQuestionEvent
      { _dqUuid = uuid
      , _dqKmUuid = kmUuid
      , _dqChapterUuid = chapterUuid
      , _dqQuestionUuid = questionUuid
      }
