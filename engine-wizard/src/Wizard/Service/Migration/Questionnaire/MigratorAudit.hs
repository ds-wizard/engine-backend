module Wizard.Service.Migration.Questionnaire.MigratorAudit where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Audit.AuditService

auditQuestionnaireMigrationCreate :: MigratorStateCreateDTO -> Questionnaire -> Questionnaire -> AppContextM ()
auditQuestionnaireMigrationCreate reqDto oldQtn newQtn =
  logAuditWithBody
    "questionnaire.migration"
    "create"
    (U.toString $ oldQtn ^. uuid)
    (M.fromList
       [ ("sourcePackageId", oldQtn ^. packageId)
       , ("targetPackageId", reqDto ^. targetPackageId)
       , ("oldQuestionnaireUuid", U.toString $ oldQtn ^. uuid)
       , ("newQuestionnaireUuid", U.toString $ newQtn ^. uuid)
       ])

auditQuestionnaireMigrationModify :: MigratorStateDTO -> MigratorStateChangeDTO -> AppContextM ()
auditQuestionnaireMigrationModify state resolvedQuestionUuids =
  logAuditWithBody
    "questionnaire.migration"
    "modify"
    (U.toString $ state ^. oldQuestionnaire . uuid)
    (M.fromList [("resolvedQuestionUuids", show resolvedQuestionUuids)])

auditQuestionnaireMigrationFinish :: Questionnaire -> Questionnaire -> AppContextM ()
auditQuestionnaireMigrationFinish oldQtn newQtn =
  logAuditWithBody
    "questionnaire.migration"
    "finish"
    (U.toString $ oldQtn ^. uuid)
    (M.fromList
       [("oldQuestionnaireUuid", U.toString $ oldQtn ^. uuid), ("newQuestionnaireUuid", U.toString $ newQtn ^. uuid)])

auditQuestionnaireMigrationCancel :: MigratorStateDTO -> AppContextM ()
auditQuestionnaireMigrationCancel state =
  logAudit "questionnaire.migration" "cancel" (U.toString $ state ^. oldQuestionnaire . uuid)
