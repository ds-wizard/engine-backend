module Wizard.Service.Questionnaire.Migration.MigrationAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Questionnaire.Questionnaire

auditQuestionnaireMigrationCreate :: MigratorStateCreateDTO -> Questionnaire -> Questionnaire -> AppContextM ()
auditQuestionnaireMigrationCreate reqDto oldQtn newQtn =
  logAuditWithBody
    "questionnaire.migration"
    "create"
    (U.toString oldQtn.uuid)
    ( M.fromList
        [ ("sourceKnowledgeModelPackageId", oldQtn.knowledgeModelPackageId)
        , ("targetKnowledgeModelPackageId", reqDto.targetKnowledgeModelPackageId)
        , ("oldQuestionnaireUuid", U.toString $ oldQtn.uuid)
        , ("newQuestionnaireUuid", U.toString $ newQtn.uuid)
        ]
    )

auditQuestionnaireMigrationModify :: MigratorStateDTO -> MigratorStateChangeDTO -> AppContextM ()
auditQuestionnaireMigrationModify state resolvedQuestionUuids =
  logAuditWithBody
    "questionnaire.migration"
    "modify"
    (U.toString $ state.oldQuestionnaire.uuid)
    (M.fromList [("resolvedQuestionUuids", show resolvedQuestionUuids)])

auditQuestionnaireMigrationFinish :: Questionnaire -> Questionnaire -> AppContextM ()
auditQuestionnaireMigrationFinish oldQtn newQtn =
  logAuditWithBody
    "questionnaire.migration"
    "finish"
    (U.toString oldQtn.uuid)
    ( M.fromList
        [("oldQuestionnaireUuid", U.toString $ oldQtn.uuid), ("newQuestionnaireUuid", U.toString $ newQtn.uuid)]
    )

auditQuestionnaireMigrationCancel :: MigratorStateDTO -> AppContextM ()
auditQuestionnaireMigrationCancel state =
  logAudit "questionnaire.migration" "cancel" (U.toString $ state.oldQuestionnaire.uuid)
