module Wizard.Api.Handler.App.Plan.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Plan.AppPlanChangeDTO
import Wizard.Api.Resource.Plan.AppPlanChangeJM ()
import Wizard.Api.Resource.Plan.AppPlanJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Plan.AppPlan
import Wizard.Service.Plan.AppPlanService

type List_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] AppPlanChangeDTO
     :> "apps"
     :> Capture "aUuid" U.UUID
     :> "plans"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppPlan)

list_POST ::
     Maybe String
  -> Maybe String
  -> AppPlanChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppPlan)
list_POST mTokenHeader mServerUrl reqDto appUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createPlan appUuid reqDto
