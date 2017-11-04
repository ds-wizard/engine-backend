module Database.BSON.Event.FollowUpQuestion where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent

-- ----------------------------------
-- ADD FOLLOW-UP QUESTION EVENT -----
-- ----------------------------------
instance ToBSON AddFollowUpQuestionEvent where
  toBSON event =
    [ "uuid" BSON.=: serializeUUID (event ^. afuqUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. afuqKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. afuqChapterUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. afuqAnswerUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. afuqQuestionUuid)
    , "qType" BSON.=: (event ^. afuqType)
    , "title" BSON.=: (event ^. afuqTitle)
    , "text" BSON.=: (event ^. afuqText)
    ]

instance FromBSON AddFollowUpQuestionEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    answerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    qType <- BSON.lookup "qType" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    return
      AddFollowUpQuestionEvent
      { _afuqUuid = uuid
      , _afuqKmUuid = kmUuid
      , _afuqChapterUuid = chapterUuid
      , _afuqAnswerUuid = answerUuid
      , _afuqQuestionUuid = questionUuid
      , _afuqType = qType
      , _afuqTitle = title
      , _afuqText = text
      }

-- ----------------------------------
-- EDIT FOLLOW-UP QUESTION EVENT ----
-- ----------------------------------
instance ToBSON EditFollowUpQuestionEvent where
  toBSON event =
    [ "uuid" BSON.=: serializeUUID (event ^. efuqUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. efuqKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. efuqChapterUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. efuqAnswerUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. efuqQuestionUuid)
    , "qType" BSON.=: (event ^. efuqType)
    , "title" BSON.=: (event ^. efuqTitle)
    , "text" BSON.=: (event ^. efuqText)
    , "answerUuids" BSON.=: serializeMaybeUUIDList (event ^. efuqAnswerIds)
    , "expertUuids" BSON.=: serializeMaybeUUIDList (event ^. efuqExpertIds)
    , "referenceUuids" BSON.=:
      serializeMaybeUUIDList (event ^. efuqReferenceIds)
    ]

instance FromBSON EditFollowUpQuestionEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    answerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    qType <- BSON.lookup "qType" doc
    title <- BSON.lookup "title" doc
    text <- BSON.lookup "text" doc
    let answerUuids = deserializeMaybeUUIDList $ BSON.lookup "answerUuids" doc
    let expertUuids = deserializeMaybeUUIDList $ BSON.lookup "expertUuids" doc
    let referenceUuids =
          deserializeMaybeUUIDList $ BSON.lookup "referenceUuids" doc
    return
      EditFollowUpQuestionEvent
      { _efuqUuid = uuid
      , _efuqKmUuid = kmUuid
      , _efuqChapterUuid = chapterUuid
      , _efuqAnswerUuid = answerUuid
      , _efuqQuestionUuid = questionUuid
      , _efuqType = qType
      , _efuqTitle = title
      , _efuqText = text
      , _efuqAnswerIds = answerUuids
      , _efuqExpertIds = expertUuids
      , _efuqReferenceIds = referenceUuids
      }

-- ----------------------------------
-- DELETE FOLLOW-UP QUESTION EVENT --
-- ----------------------------------
instance ToBSON DeleteFollowUpQuestionEvent where
  toBSON event =
    [ "uuid" BSON.=: serializeUUID (event ^. dfuqUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. dfuqKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. dfuqChapterUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. dfuqAnswerUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. dfuqQuestionUuid)
    ]

instance FromBSON DeleteFollowUpQuestionEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    answerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    return
      DeleteFollowUpQuestionEvent
      { _dfuqUuid = uuid
      , _dfuqKmUuid = kmUuid
      , _dfuqChapterUuid = chapterUuid
      , _dfuqAnswerUuid = answerUuid
      , _dfuqQuestionUuid = questionUuid
      }
