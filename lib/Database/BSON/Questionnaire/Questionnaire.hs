module Database.BSON.Questionnaire.Questionnaire where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.BSON.Questionnaire.QuestionnaireReply ()
import LensesConfig
import Model.Questionnaire.Questionnaire

instance ToBSON Questionnaire where
  toBSON questionnaire =
    [ "uuid" BSON.=: serializeUUID (questionnaire ^. uuid)
    , "name" BSON.=: (questionnaire ^. name)
    , "level" BSON.=: (questionnaire ^. level)
    , "private" BSON.=: (questionnaire ^. private)
    , "packageId" BSON.=: (questionnaire ^. packageId)
    , "knowledgeModel" BSON.=: (questionnaire ^. knowledgeModel)
    , "replies" BSON.=: (questionnaire ^. replies)
    , "ownerUuid" BSON.=: serializeMaybeUUID (questionnaire ^. ownerUuid)
    , "createdAt" BSON.=: (questionnaire ^. createdAt)
    , "updatedAt" BSON.=: (questionnaire ^. updatedAt)
    ]

instance FromBSON Questionnaire where
  fromBSON doc = do
    uuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    level <- BSON.lookup "level" doc
    private <- BSON.lookup "private" doc
    packageId <- BSON.lookup "packageId" doc
    knowledgeModel <- BSON.lookup "knowledgeModel" doc
    replies <- BSON.lookup "replies" doc
    let ownerUuid = deserializeMaybeUUID $ BSON.lookup "ownerUuid" doc
    createdAt <- BSON.lookup "createdAt" doc
    updatedAt <- BSON.lookup "updatedAt" doc
    return
      Questionnaire
      { _questionnaireUuid = uuid
      , _questionnaireName = name
      , _questionnaireLevel = level
      , _questionnairePrivate = private
      , _questionnairePackageId = packageId
      , _questionnaireKnowledgeModel = knowledgeModel
      , _questionnaireReplies = replies
      , _questionnaireOwnerUuid = ownerUuid
      , _questionnaireCreatedAt = createdAt
      , _questionnaireUpdatedAt = updatedAt
      }
