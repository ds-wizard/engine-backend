module Wizard.Api.Handler.Tenant.Usage.Detail_Wizard_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Usage.UsageService
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM ()

type Detail_Wizard_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> Capture "uuid" U.UUID
    :> "usages"
    :> "wizard"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)

detail_wizard_GET :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)
detail_wizard_GET mTokenHeader mServerUrl tenantUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUsage tenantUuid
