module Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toVersionDTO :: QuestionnaireVersion -> Maybe User -> QuestionnaireVersionDTO
toVersionDTO version mCreatedBy =
  QuestionnaireVersionDTO
    { uuid = version.uuid
    , name = version.name
    , description = version.description
    , eventUuid = version.eventUuid
    , createdBy = fmap (UM.toSuggestionDTO . UM.toSuggestion) mCreatedBy
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
fromVersionChangeDTO :: QuestionnaireVersionChangeDTO -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> QuestionnaireVersion
fromVersionChangeDTO version uuid createdBy createdAt updatedAt =
  QuestionnaireVersion
    { uuid = uuid
    , name = version.name
    , description = version.description
    , eventUuid = version.eventUuid
    , createdBy = createdBy
    , createdAt = createdAt
    , updatedAt = updatedAt
    }
