module Wizard.Service.QuestionnaireImporter.QuestionnaireImporterAudit where

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditQuestionnaireImporterStartEvent :: String -> AppContextM ()
auditQuestionnaireImporterStartEvent = logAudit "questionnaireImporter" "startEvent"
