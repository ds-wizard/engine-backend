module Database.BSON.Questionnaire.Questionnaire where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe ()

import Database.BSON.Common ()
import Database.BSON.Questionnaire.QuestionnaireAccessibility ()
import Database.BSON.Questionnaire.QuestionnaireLabel ()
import Database.BSON.Questionnaire.QuestionnaireReply ()
import Model.Questionnaire.Questionnaire

instance ToBSON Questionnaire where
  toBSON Questionnaire {..} =
    [ "uuid" BSON.=: _questionnaireUuid
    , "name" BSON.=: _questionnaireName
    , "level" BSON.=: _questionnaireLevel
    , "accessibility" BSON.=: _questionnaireAccessibility
    , "packageId" BSON.=: _questionnairePackageId
    , "selectedTagUuids" BSON.=: _questionnaireSelectedTagUuids
    , "ownerUuid" BSON.=: _questionnaireOwnerUuid
    , "replies" BSON.=: _questionnaireReplies
    , "labels" BSON.=: _questionnaireLabels
    , "createdAt" BSON.=: _questionnaireCreatedAt
    , "updatedAt" BSON.=: _questionnaireUpdatedAt
    ]

instance FromBSON Questionnaire where
  fromBSON doc = do
    _questionnaireUuid <- BSON.lookup "uuid" doc
    _questionnaireName <- BSON.lookup "name" doc
    _questionnaireLevel <- BSON.lookup "level" doc
    _questionnaireAccessibility <- BSON.lookup "accessibility" doc
    _questionnairePackageId <- BSON.lookup "packageId" doc
    _questionnaireSelectedTagUuids <- BSON.lookup "selectedTagUuids" doc
    let _questionnaireOwnerUuid = BSON.lookup "ownerUuid" doc
    _questionnaireReplies <- BSON.lookup "replies" doc
    _questionnaireLabels <- BSON.lookup "labels" doc
    _questionnaireCreatedAt <- BSON.lookup "createdAt" doc
    _questionnaireUpdatedAt <- BSON.lookup "updatedAt" doc
    return Questionnaire {..}
