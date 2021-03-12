module Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as UM

toVersionDTO :: QuestionnaireVersion -> User -> QuestionnaireVersionDTO
toVersionDTO version createdBy =
  QuestionnaireVersionDTO
    { _questionnaireVersionDTOUuid = version ^. uuid
    , _questionnaireVersionDTOName = version ^. name
    , _questionnaireVersionDTODescription = version ^. description
    , _questionnaireVersionDTOEventUuid = version ^. eventUuid
    , _questionnaireVersionDTOCreatedBy = UM.toSuggestionDTO . UM.toSuggestion $ createdBy
    , _questionnaireVersionDTOCreatedAt = version ^. createdAt
    , _questionnaireVersionDTOUpdatedAt = version ^. updatedAt
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toVersionChangeDTO :: QuestionnaireVersion -> QuestionnaireVersionChangeDTO
toVersionChangeDTO version =
  QuestionnaireVersionChangeDTO
    { _questionnaireVersionChangeDTOName = version ^. name
    , _questionnaireVersionChangeDTODescription = version ^. description
    , _questionnaireVersionChangeDTOEventUuid = version ^. eventUuid
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toVersionRevertDTO :: U.UUID -> QuestionnaireVersionRevertDTO
toVersionRevertDTO eventUuid = QuestionnaireVersionRevertDTO {_questionnaireVersionRevertDTOEventUuid = eventUuid}

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
fromVersionChangeDTO :: QuestionnaireVersionChangeDTO -> U.UUID -> U.UUID -> UTCTime -> UTCTime -> QuestionnaireVersion
fromVersionChangeDTO version uuid createdBy createdAt updatedAt =
  QuestionnaireVersion
    { _questionnaireVersionUuid = uuid
    , _questionnaireVersionName = version ^. name
    , _questionnaireVersionDescription = version ^. description
    , _questionnaireVersionEventUuid = version ^. eventUuid
    , _questionnaireVersionCreatedBy = createdBy
    , _questionnaireVersionCreatedAt = createdAt
    , _questionnaireVersionUpdatedAt = updatedAt
    }
