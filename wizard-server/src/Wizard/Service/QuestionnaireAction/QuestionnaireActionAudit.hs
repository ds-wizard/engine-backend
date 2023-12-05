module Wizard.Service.QuestionnaireAction.QuestionnaireActionAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditQuestionnaireActionStartEvent :: String -> AppContextM ()
auditQuestionnaireActionStartEvent = logAudit "questionnaireAction" "startEvent"
