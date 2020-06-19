module Wizard.Database.BSON.Questionnaire.Questionnaire where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe ()

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireLabel ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireReply ()
import Wizard.Database.BSON.Questionnaire.QuestionnaireVisibility ()
import Wizard.Model.Questionnaire.Questionnaire

instance ToBSON Questionnaire where
  toBSON Questionnaire {..} =
    [ "uuid" BSON.=: _questionnaireUuid
    , "name" BSON.=: _questionnaireName
    , "level" BSON.=: _questionnaireLevel
    , "visibility" BSON.=: _questionnaireVisibility
    , "packageId" BSON.=: _questionnairePackageId
    , "selectedTagUuids" BSON.=: _questionnaireSelectedTagUuids
    , "templateId" BSON.=: _questionnaireTemplateId
    , "formatUuid" BSON.=: _questionnaireFormatUuid
    , "ownerUuid" BSON.=: _questionnaireOwnerUuid
    , "creatorUuid" BSON.=: _questionnaireCreatorUuid
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
    _questionnaireVisibility <- BSON.lookup "visibility" doc
    _questionnairePackageId <- BSON.lookup "packageId" doc
    _questionnaireSelectedTagUuids <- BSON.lookup "selectedTagUuids" doc
    let _questionnaireTemplateId = BSON.lookup "templateId" doc
    let _questionnaireFormatUuid = BSON.lookup "formatUuid" doc
    let _questionnaireOwnerUuid = BSON.lookup "ownerUuid" doc
    let _questionnaireCreatorUuid = BSON.lookup "creatorUuid" doc
    _questionnaireReplies <- BSON.lookup "replies" doc
    _questionnaireLabels <- BSON.lookup "labels" doc
    _questionnaireCreatedAt <- BSON.lookup "createdAt" doc
    _questionnaireUpdatedAt <- BSON.lookup "updatedAt" doc
    return Questionnaire {..}
