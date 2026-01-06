module Wizard.Service.Project.Action.ProjectActionAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditProjectActionStartEvent :: String -> AppContextM ()
auditProjectActionStartEvent = logAudit "projectAction" "startEvent"
