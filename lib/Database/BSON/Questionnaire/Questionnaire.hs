module Database.BSON.Questionnaire.Questionnaire where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import LensesConfig
import Model.Questionnaire.Questionnaire

instance ToBSON QuestionnaireReply where
  toBSON reply = ["path" BSON.=: (reply ^. path), "value" BSON.=: (reply ^. value)]

instance FromBSON QuestionnaireReply where
  fromBSON doc = do
    path <- BSON.lookup "path" doc
    value <- BSON.lookup "value" doc
    return QuestionnaireReply {_questionnaireReplyPath = path, _questionnaireReplyValue = value}

instance ToBSON Questionnaire where
  toBSON questionnaire =
    [ "uuid" BSON.=: serializeUUID (questionnaire ^. uuid)
    , "name" BSON.=: (questionnaire ^. name)
    , "packageId" BSON.=: (questionnaire ^. packageId)
    , "knowledgeModel" BSON.=: (questionnaire ^. knowledgeModel)
    , "replies" BSON.=: (questionnaire ^. replies)
    , "createdAt" BSON.=: (questionnaire ^. createdAt)
    , "updatedAt" BSON.=: (questionnaire ^. updatedAt)
    ]

instance FromBSON Questionnaire where
  fromBSON doc = do
    uuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    packageId <- BSON.lookup "packageId" doc
    knowledgeModel <- BSON.lookup "knowledgeModel" doc
    replies <- BSON.lookup "replies" doc
    createdAt <- BSON.lookup "createdAt" doc
    updatedAt <- BSON.lookup "updatedAt" doc
    return
      Questionnaire
      { _questionnaireUuid = uuid
      , _questionnaireName = name
      , _questionnairePackageId = packageId
      , _questionnaireKnowledgeModel = knowledgeModel
      , _questionnaireReplies = replies
      , _questionnaireCreatedAt = createdAt
      , _questionnaireUpdatedAt = updatedAt
      }
