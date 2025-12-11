module Wizard.Service.Project.ProjectAudit where

import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditProjectListEvents :: U.UUID -> AppContextM ()
auditProjectListEvents projectUuid = logAudit "project" "listEvents" (U.toString projectUuid)

auditProjectDetailEvent :: U.UUID -> AppContextM ()
auditProjectDetailEvent projectUuid = logAudit "project" "detailEvent" (U.toString projectUuid)
