module Wizard.Service.Migration.Questionnaire.MigratorMapper where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Model.Migration.Questionnaire.MigratorState

toDTO :: QuestionnaireDetailDTO -> QuestionnaireDetailDTO -> [U.UUID] -> U.UUID -> MigratorStateDTO
toDTO oldQtn newQtn qtnUuids appUuid =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire = oldQtn
    , _migratorStateDTONewQuestionnaire = newQtn
    , _migratorStateDTOResolvedQuestionUuids = qtnUuids
    , _migratorStateDTOAppUuid = appUuid
    }

fromCreateDTO :: U.UUID -> U.UUID -> U.UUID -> MigratorState
fromCreateDTO oldQtnUuid newQtnUuid appUuid =
  MigratorState
    { _migratorStateOldQuestionnaireUuid = oldQtnUuid
    , _migratorStateNewQuestionnaireUuid = newQtnUuid
    , _migratorStateResolvedQuestionUuids = []
    , _migratorStateAppUuid = appUuid
    }

fromChangeDTO :: MigratorStateChangeDTO -> MigratorStateDTO -> MigratorState
fromChangeDTO changeDto ms =
  MigratorState
    { _migratorStateOldQuestionnaireUuid = ms ^. oldQuestionnaire . uuid
    , _migratorStateNewQuestionnaireUuid = ms ^. newQuestionnaire . uuid
    , _migratorStateResolvedQuestionUuids = changeDto ^. resolvedQuestionUuids
    , _migratorStateAppUuid = ms ^. appUuid
    }
