module Wizard.Api.Handler.Dev.Operation.List_Executions_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Dev.DevExecutionDTO
import Shared.Common.Api.Resource.Dev.DevExecutionJM ()
import Shared.Common.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Common.Api.Resource.Dev.DevExecutionResultJM ()
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Service.Dev.DevOperationService
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Dev.DevOperationDefinitions

type List_Executions_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DevExecutionDTO
    :> "dev-operations"
    :> "executions"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] AdminExecutionResultDTO)

list_executions_POST
  :: Maybe String
  -> Maybe String
  -> DevExecutionDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] AdminExecutionResultDTO)
list_executions_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< executeOperation sections reqDto
