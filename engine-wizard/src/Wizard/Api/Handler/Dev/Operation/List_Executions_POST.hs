module Wizard.Api.Handler.Dev.Operation.List_Executions_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Api.Resource.Dev.DevExecutionJM ()
import Wizard.Api.Resource.Dev.DevExecutionResultDTO
import Wizard.Api.Resource.Dev.DevExecutionResultJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Dev.DevOperationService

type List_Executions_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] DevExecutionDTO
     :> "dev-operations"
     :> "executions"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AdminExecutionResultDTO)

list_executions_POST ::
     Maybe String
  -> Maybe String
  -> DevExecutionDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AdminExecutionResultDTO)
list_executions_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< executeOperation reqDto
