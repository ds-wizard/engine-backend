module Database.BSON.Questionnaire.Questionnaire where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe ()

import Database.BSON.Common
import Database.BSON.Questionnaire.QuestionnaireReply ()
import Model.Questionnaire.Questionnaire

instance ToBSON Questionnaire where
  toBSON Questionnaire {..} =
    [ "uuid" BSON.=: serializeUUID _questionnaireUuid
    , "name" BSON.=: _questionnaireName
    , "level" BSON.=: _questionnaireLevel
    , "accessibility" BSON.=: serializeQuestionnaireAccessibility _questionnaireAccessibility
    , "packageId" BSON.=: _questionnairePackageId
    , "selectedTagUuids" BSON.=: serializeUUIDList _questionnaireSelectedTagUuids
    , "ownerUuid" BSON.=: serializeMaybeUUID _questionnaireOwnerUuid
    , "replies" BSON.=: _questionnaireReplies
    , "createdAt" BSON.=: _questionnaireCreatedAt
    , "updatedAt" BSON.=: _questionnaireUpdatedAt
    ]

instance FromBSON Questionnaire where
  fromBSON doc = do
    _questionnaireUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    _questionnaireName <- BSON.lookup "name" doc
    _questionnaireLevel <- BSON.lookup "level" doc
    _questionnaireAccessibility <- deserializeQuestionnaireAccessibility $ BSON.lookup "accessibility" doc
    _questionnairePackageId <- BSON.lookup "packageId" doc
    _questionnaireSelectedTagUuids <- deserializeMaybeUUIDList $ BSON.lookup "selectedTagUuids" doc
    let _questionnaireOwnerUuid = deserializeMaybeUUID $ BSON.lookup "ownerUuid" doc
    _questionnaireReplies <- BSON.lookup "replies" doc
    _questionnaireCreatedAt <- BSON.lookup "createdAt" doc
    _questionnaireUpdatedAt <- BSON.lookup "updatedAt" doc
    return Questionnaire {..}
