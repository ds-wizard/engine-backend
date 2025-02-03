module Wizard.Service.Migration.Questionnaire.MigratorMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.QuestionnaireEvent

toDTO :: QuestionnaireDetailQuestionnaireDTO -> QuestionnaireDetailQuestionnaireDTO -> [U.UUID] -> U.UUID -> MigratorStateDTO
toDTO oldQtn newQtn qtnUuids tenantUuid =
  MigratorStateDTO
    { oldQuestionnaire = oldQtn
    , newQuestionnaire = newQtn
    , resolvedQuestionUuids = qtnUuids
    , tenantUuid = tenantUuid
    }

fromCreateDTO :: U.UUID -> U.UUID -> U.UUID -> MigratorState
fromCreateDTO oldQtnUuid newQtnUuid tenantUuid =
  MigratorState
    { oldQuestionnaireUuid = oldQtnUuid
    , newQuestionnaireUuid = newQtnUuid
    , resolvedQuestionUuids = []
    , tenantUuid = tenantUuid
    }

fromChangeDTO :: MigratorStateChangeDTO -> MigratorStateDTO -> MigratorState
fromChangeDTO changeDto ms =
  MigratorState
    { oldQuestionnaireUuid = ms.oldQuestionnaire.uuid
    , newQuestionnaireUuid = ms.newQuestionnaire.uuid
    , resolvedQuestionUuids = changeDto.resolvedQuestionUuids
    , tenantUuid = ms.tenantUuid
    }

toPhaseEvent :: U.UUID -> Maybe U.UUID -> U.UUID -> U.UUID -> Maybe UserDTO -> UTCTime -> QuestionnaireEvent
toPhaseEvent phaseEventUuid kmPhaseUuid questionnaireUuid tenantUuid mCurrentUserUuid now =
  SetPhaseEvent' $
    SetPhaseEvent
      { uuid = phaseEventUuid
      , phaseUuid = kmPhaseUuid
      , questionnaireUuid = questionnaireUuid
      , tenantUuid = tenantUuid
      , createdBy = fmap (.uuid) mCurrentUserUuid
      , createdAt = now
      }
