module Wizard.Service.Migration.Questionnaire.MigratorMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.QuestionnaireEvent

toDTO :: QuestionnaireDetailDTO -> QuestionnaireDetailDTO -> [U.UUID] -> U.UUID -> MigratorStateDTO
toDTO oldQtn newQtn qtnUuids appUuid =
  MigratorStateDTO
    { oldQuestionnaire = oldQtn
    , newQuestionnaire = newQtn
    , resolvedQuestionUuids = qtnUuids
    , appUuid = appUuid
    }

fromCreateDTO :: U.UUID -> U.UUID -> U.UUID -> MigratorState
fromCreateDTO oldQtnUuid newQtnUuid appUuid =
  MigratorState
    { oldQuestionnaireUuid = oldQtnUuid
    , newQuestionnaireUuid = newQtnUuid
    , resolvedQuestionUuids = []
    , appUuid = appUuid
    }

fromChangeDTO :: MigratorStateChangeDTO -> MigratorStateDTO -> MigratorState
fromChangeDTO changeDto ms =
  MigratorState
    { oldQuestionnaireUuid = ms.oldQuestionnaire.uuid
    , newQuestionnaireUuid = ms.newQuestionnaire.uuid
    , resolvedQuestionUuids = changeDto.resolvedQuestionUuids
    , appUuid = ms.appUuid
    }

toPhaseEvent :: U.UUID -> U.UUID -> Maybe UserDTO -> UTCTime -> QuestionnaireEvent
toPhaseEvent phaseEventUuid kmPhaseUuid mCurrentUserUuid now =
  SetPhaseEvent' $
    SetPhaseEvent
      { uuid = phaseEventUuid
      , phaseUuid = Just kmPhaseUuid
      , createdBy = fmap (.uuid) mCurrentUserUuid
      , createdAt = now
      }
