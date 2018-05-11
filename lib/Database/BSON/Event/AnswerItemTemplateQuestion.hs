module Database.BSON.Event.AnswerItemTemplateQuestion where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.Event.EventField ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import LensesConfig
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent

-- ---------------------------------------------
-- ADD ANSWER ITEM TEMPLATE QUESTION EVENT -----
-- ---------------------------------------------
instance ToBSON AddAnswerItemTemplateQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddAnswerItemTemplateQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "parentQuestionUuid" BSON.=: serializeUUID (event ^. parentQuestionUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "shortQuestionUuid" BSON.=: (event ^. shortQuestionUuid)
    , "qType" BSON.=: serializeQuestionType (event ^. qType)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "aitAnswerItemTemplatePlain" BSON.=: (event ^. answerItemTemplatePlain)
    ]

instance FromBSON AddAnswerItemTemplateQuestionEvent where
  fromBSON doc = do
    aitUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    aitKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    aitChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    aitParentQuestionUuid <- deserializeUUID $ BSON.lookup "parentQuestionUuid" doc
    aitQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    aitShortQuestionUuid <- BSON.lookup "shortQuestionUuid" doc
    aitQType <- deserializeQuestionType $ BSON.lookup "qType" doc
    aitTitle <- BSON.lookup "title" doc
    aitText <- BSON.lookup "text" doc
    aitAnswerItemTemplatePlain <- BSON.lookup "aitAnswerItemTemplatePlain" doc
    return
      AddAnswerItemTemplateQuestionEvent
      { _addAnswerItemTemplateQuestionEventUuid = aitUuid
      , _addAnswerItemTemplateQuestionEventKmUuid = aitKmUuid
      , _addAnswerItemTemplateQuestionEventChapterUuid = aitChapterUuid
      , _addAnswerItemTemplateQuestionEventParentQuestionUuid = aitParentQuestionUuid
      , _addAnswerItemTemplateQuestionEventQuestionUuid = aitQuestionUuid
      , _addAnswerItemTemplateQuestionEventShortQuestionUuid = aitShortQuestionUuid
      , _addAnswerItemTemplateQuestionEventQType = aitQType
      , _addAnswerItemTemplateQuestionEventTitle = aitTitle
      , _addAnswerItemTemplateQuestionEventText = aitText
      , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain = aitAnswerItemTemplatePlain
      }

-- ---------------------------------------------
-- EDIT ANSWER ITEM TEMPLATE QUESTION EVENT ----
-- ---------------------------------------------
instance ToBSON EditAnswerItemTemplateQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditAnswerItemTemplateQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "parentQuestionUuid" BSON.=: serializeUUID (event ^. parentQuestionUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    , "shortQuestionUuid" BSON.=: (event ^. shortQuestionUuid)
    , "qType" BSON.=: (event ^. qType)
    , "title" BSON.=: (event ^. title)
    , "text" BSON.=: (event ^. text)
    , "answerItemTemplatePlainWithIds" BSON.=: (event ^. answerItemTemplatePlainWithIds)
    , "answerIds" BSON.=: serializeEventFieldMaybeUUIDList (event ^. answerIds)
    , "expertIds" BSON.=: serializeEventFieldUUIDList (event ^. expertIds)
    , "referenceIds" BSON.=: serializeEventFieldUUIDList (event ^. referenceIds)
    ]

instance FromBSON EditAnswerItemTemplateQuestionEvent where
  fromBSON doc = do
    aitUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    aitKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    aitChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    aitParentQuestionUuid <- deserializeUUID $ BSON.lookup "parentQuestionUuid" doc
    aitQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    aitShortQuestionUuid <- BSON.lookup "shortQuestionUuid" doc
    aitQType <- BSON.lookup "qType" doc
    aitTitle <- BSON.lookup "title" doc
    aitText <- BSON.lookup "text" doc
    answerItemTemplatePlainWithIds <- BSON.lookup "answerItemTemplatePlainWithIds" doc
    let aitAnswerIds = deserializeEventFieldMaybeUUIDList $ BSON.lookup "answerIds" doc
    let aitExpertIds = deserializeEventFieldUUIDList $ BSON.lookup "expertIds" doc
    let aitReferenceIds = deserializeEventFieldUUIDList $ BSON.lookup "referenceIds" doc
    return
      EditAnswerItemTemplateQuestionEvent
      { _editAnswerItemTemplateQuestionEventUuid = aitUuid
      , _editAnswerItemTemplateQuestionEventKmUuid = aitKmUuid
      , _editAnswerItemTemplateQuestionEventChapterUuid = aitChapterUuid
      , _editAnswerItemTemplateQuestionEventParentQuestionUuid = aitParentQuestionUuid
      , _editAnswerItemTemplateQuestionEventQuestionUuid = aitQuestionUuid
      , _editAnswerItemTemplateQuestionEventShortQuestionUuid = aitShortQuestionUuid
      , _editAnswerItemTemplateQuestionEventQType = aitQType
      , _editAnswerItemTemplateQuestionEventTitle = aitTitle
      , _editAnswerItemTemplateQuestionEventText = aitText
      , _editAnswerItemTemplateQuestionEventAnswerItemTemplatePlainWithIds = answerItemTemplatePlainWithIds
      , _editAnswerItemTemplateQuestionEventAnswerIds = aitAnswerIds
      , _editAnswerItemTemplateQuestionEventExpertIds = aitExpertIds
      , _editAnswerItemTemplateQuestionEventReferenceIds = aitReferenceIds
      }

-- ---------------------------------------------
-- DELETE ANSWER ITEM TEMPLATE QUESTION EVENT --
-- ---------------------------------------------
instance ToBSON DeleteAnswerItemTemplateQuestionEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteAnswerItemTemplateQuestionEvent"
    , "uuid" BSON.=: serializeUUID (event ^. uuid)
    , "kmUuid" BSON.=: serializeUUID (event ^. kmUuid)
    , "chapterUuid" BSON.=: serializeUUID (event ^. chapterUuid)
    , "parentQuestionUuid" BSON.=: serializeUUID (event ^. parentQuestionUuid)
    , "questionUuid" BSON.=: serializeUUID (event ^. questionUuid)
    ]

instance FromBSON DeleteAnswerItemTemplateQuestionEvent where
  fromBSON doc = do
    aitUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    aitKmUuid <- deserializeUUID $ BSON.lookup "kmUuid" doc
    aitChapterUuid <- deserializeUUID $ BSON.lookup "chapterUuid" doc
    aitParentQuestionUuid <- deserializeUUID $ BSON.lookup "parentQuestionUuid" doc
    aitQuestionUuid <- deserializeUUID $ BSON.lookup "questionUuid" doc
    return
      DeleteAnswerItemTemplateQuestionEvent
      { _deleteAnswerItemTemplateQuestionEventUuid = aitUuid
      , _deleteAnswerItemTemplateQuestionEventKmUuid = aitKmUuid
      , _deleteAnswerItemTemplateQuestionEventChapterUuid = aitChapterUuid
      , _deleteAnswerItemTemplateQuestionEventParentQuestionUuid = aitParentQuestionUuid
      , _deleteAnswerItemTemplateQuestionEventQuestionUuid = aitQuestionUuid
      }
