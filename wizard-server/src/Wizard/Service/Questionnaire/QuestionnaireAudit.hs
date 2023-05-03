module Wizard.Service.Questionnaire.QuestionnaireAudit where

import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditQuestionnaireListEvents :: U.UUID -> AppContextM ()
auditQuestionnaireListEvents qtnUuid = logAudit "questionnaire" "listEvents" (U.toString qtnUuid)

auditQuestionnaireDetailEvent :: U.UUID -> AppContextM ()
auditQuestionnaireDetailEvent qtnUuid = logAudit "questionnaire" "detailEvent" (U.toString qtnUuid)
