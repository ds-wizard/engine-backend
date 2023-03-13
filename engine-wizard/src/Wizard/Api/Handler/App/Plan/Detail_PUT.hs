module Wizard.Api.Handler.App.Plan.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Plan.AppPlanChangeDTO
import Wizard.Api.Resource.Plan.AppPlanChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Plan.AppPlan
import Wizard.Service.Plan.AppPlanService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] AppPlanChangeDTO
    :> "apps"
    :> Capture "aUuid" U.UUID
    :> "plans"
    :> Capture "pUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] AppPlan)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> AppPlanChangeDTO
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] AppPlan)
detail_PUT mTokenHeader mServerUrl reqDto aUuid pUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPlan aUuid pUuid reqDto
