module Database.BSON.Event.Answer where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent

-- -------------------------
-- ADD ANSWER EVENT --------
-- -------------------------
instance ToBSON AddAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. aansUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. aansKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. aansChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. aansQuestionUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. aansAnswerUuid)
    , "label" BSON.=: (event ^. aansLabel)
    , "advice" BSON.=: (event ^. aansAdvice)
    ]

instance FromBSON AddAnswerEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    answerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    label <- BSON.lookup "label" doc
    advice <- BSON.lookup "advice" doc
    return
      AddAnswerEvent
      { _aansUuid = uuid
      , _aansKmUuid = kmUuid
      , _aansChapterUuid = chapterUuid
      , _aansQuestionUuid = questionUuid
      , _aansAnswerUuid = answerUuid
      , _aansLabel = label
      , _aansAdvice = advice
      }
      -- -------------------------

-- EDIT ANSWER EVENT -------
-- -------------------------
instance ToBSON EditAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. eansUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. eansKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. eansChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. eansQuestionUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. eansAnswerUuid)
    , "label" BSON.=: (event ^. eansLabel)
    , "advice" BSON.=: (event ^. eansAdvice)
    , "followingIds" BSON.=: serializeMaybeUUIDList (event ^. eansFollowingIds)
    ]

instance FromBSON EditAnswerEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    answerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    label <- BSON.lookup "label" doc
    advice <- BSON.lookup "advice" doc
    let followingIds = deserializeMaybeUUIDList $ BSON.lookup "followingIds" doc
    return
      EditAnswerEvent
      { _eansUuid = uuid
      , _eansKmUuid = kmUuid
      , _eansChapterUuid = chapterUuid
      , _eansQuestionUuid = questionUuid
      , _eansAnswerUuid = answerUuid
      , _eansLabel = label
      , _eansAdvice = advice
      , _eansFollowingIds = followingIds
      }

-- -------------------------
-- DELETE ANSWER EVENT -----
-- -------------------------
instance ToBSON DeleteAnswerEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteAnswerEvent"
    , "uuid" BSON.=: serializeUUID (event ^. dansUuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. dansKmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. dansChapterUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. dansQuestionUuid)
    , "answerUuid" BSON.=: serializeUUID (event ^. dansAnswerUuid)
    ]

instance FromBSON DeleteAnswerEvent where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    kmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    chapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    questionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    answerUuid <- deserializeUUID $ BSON.lookup "answerUuid" doc
    return
      DeleteAnswerEvent
      { _dansUuid = uuid
      , _dansKmUuid = kmUuid
      , _dansChapterUuid = chapterUuid
      , _dansQuestionUuid = questionUuid
      , _dansAnswerUuid = answerUuid
      }
