module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditQuestionnaireImporterStartEvent :: String -> AppContextM ()
auditQuestionnaireImporterStartEvent = logAudit "questionnaireImporter" "startEvent"
