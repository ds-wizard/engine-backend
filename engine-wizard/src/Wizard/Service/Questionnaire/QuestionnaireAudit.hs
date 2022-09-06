module Wizard.Service.Questionnaire.QuestionnaireAudit where

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditQuestionnaireListEvents :: String -> AppContextM ()
auditQuestionnaireListEvents = logAudit "questionnaire" "listEvents"

auditQuestionnaireDetailEvent :: String -> AppContextM ()
auditQuestionnaireDetailEvent = logAudit "questionnaire" "detailEvent"
