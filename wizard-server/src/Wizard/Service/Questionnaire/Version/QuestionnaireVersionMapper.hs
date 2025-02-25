module Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import qualified Wizard.Service.User.UserMapper as UM

toVersionList :: QuestionnaireVersion -> Maybe UserDTO -> QuestionnaireVersionList
toVersionList version createdBy =
  QuestionnaireVersionList
    { uuid = version.uuid
    , name = version.name
    , description = version.description
    , eventUuid = version.eventUuid
    , createdBy = fmap UM.toSuggestionDTO' createdBy
    , createdAt = version.createdAt
    , updatedAt = version.updatedAt
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toVersionChangeDTO :: QuestionnaireVersion -> QuestionnaireVersionChangeDTO
toVersionChangeDTO version =
  QuestionnaireVersionChangeDTO
    { name = version.name
    , description = version.description
    , eventUuid = version.eventUuid
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toVersionRevertDTO :: U.UUID -> QuestionnaireVersionRevertDTO
toVersionRevertDTO eventUuid = QuestionnaireVersionRevertDTO {eventUuid = eventUuid}

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
fromVersionChangeDTO :: QuestionnaireVersionChangeDTO -> U.UUID -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> QuestionnaireVersion
fromVersionChangeDTO reqDto uuid questionnaireUuid tenantUuid createdBy now =
  QuestionnaireVersion
    { uuid = uuid
    , name = reqDto.name
    , description = reqDto.description
    , eventUuid = reqDto.eventUuid
    , questionnaireUuid = questionnaireUuid
    , tenantUuid = tenantUuid
    , createdBy = Just createdBy
    , createdAt = now
    , updatedAt = now
    }

fromVersionChangeDTO' :: QuestionnaireVersion -> QuestionnaireVersionChangeDTO -> UTCTime -> QuestionnaireVersion
fromVersionChangeDTO' version reqDto now =
  QuestionnaireVersion
    { uuid = version.uuid
    , name = reqDto.name
    , description = reqDto.description
    , eventUuid = reqDto.eventUuid
    , questionnaireUuid = version.questionnaireUuid
    , tenantUuid = version.tenantUuid
    , createdBy = version.createdBy
    , createdAt = version.createdAt
    , updatedAt = now
    }
