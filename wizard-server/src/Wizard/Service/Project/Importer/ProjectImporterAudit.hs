module Wizard.Service.Project.Importer.ProjectImporterAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditProjectImporterStartEvent :: String -> AppContextM ()
auditProjectImporterStartEvent = logAudit "project_importer" "startEvent"
