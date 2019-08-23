module Service.Migration.Questionnaire.MigratorMapper where

import Control.Lens ((^.))
import qualified Data.UUID as U

import Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import LensesConfig
import Model.Migration.Questionnaire.MigratorState

toDTO :: QuestionnaireDetailDTO -> QuestionnaireDetailDTO -> [U.UUID] -> MigratorStateDTO
toDTO oldQtn newQtn qtnUuids =
  MigratorStateDTO
  { _migratorStateDTOOldQuestionnaire = oldQtn
  , _migratorStateDTONewQuestionnaire = newQtn
  , _migratorStateDTOResolvedQuestionUuids = qtnUuids
  }

fromCreateDTO :: U.UUID -> U.UUID -> MigratorState
fromCreateDTO oldQtnUuid newQtnUuid =
  MigratorState
  { _migratorStateOldQuestionnaireUuid = oldQtnUuid
  , _migratorStateNewQuestionnaireUuid = newQtnUuid
  , _migratorStateResolvedQuestionUuids = []
  }

fromChangeDTO :: MigratorStateChangeDTO -> MigratorStateDTO -> MigratorState
fromChangeDTO changeDto ms =
  MigratorState
  { _migratorStateOldQuestionnaireUuid = ms ^. oldQuestionnaire . uuid
  , _migratorStateNewQuestionnaireUuid = ms ^. newQuestionnaire . uuid
  , _migratorStateResolvedQuestionUuids = changeDto ^. resolvedQuestionUuids
  }
