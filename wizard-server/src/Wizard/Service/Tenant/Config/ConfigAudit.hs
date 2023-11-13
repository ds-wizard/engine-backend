module Wizard.Service.Tenant.Config.ConfigAudit where

import Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditChangeColors :: U.UUID -> AppContextM ()
auditChangeColors aUuid = logAudit "tenant_config" "changeColors" (U.toString aUuid)
