module Wizard.Service.Config.App.AppConfigAudit where

import Data.UUID as U

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditAppConfigChangeColors :: U.UUID -> AppContextM ()
auditAppConfigChangeColors aUuid = logAudit "app_config" "changeColors" (U.toString aUuid)
