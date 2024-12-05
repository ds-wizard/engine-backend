module Wizard.Api.Handler.Tenant.Usage.Current_Wizard_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Usage.UsageService
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM ()

type Current_Wizard_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "usages"
    :> "wizard"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)

current_wizard_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)
current_wizard_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUsageForCurrentTenant
