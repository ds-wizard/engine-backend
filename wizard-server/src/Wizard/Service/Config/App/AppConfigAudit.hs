module Wizard.Service.Config.App.AppConfigAudit where

import Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditAppConfigChangeColors :: U.UUID -> AppContextM ()
auditAppConfigChangeColors aUuid = logAudit "app_config" "changeColors" (U.toString aUuid)
