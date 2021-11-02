module Wizard.Api.Handler.Admin.List_Operations_Executions_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Admin.AdminExecutionDTO
import Wizard.Api.Resource.Admin.AdminExecutionJM ()
import Wizard.Api.Resource.Admin.AdminExecutionResultDTO
import Wizard.Api.Resource.Admin.AdminExecutionResultJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Admin.AdminService

type List_Operations_Executions_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AdminExecutionDTO
     :> "admin"
     :> "operations"
     :> "executions"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AdminExecutionResultDTO)

list_operations_executions_POST ::
     Maybe String
  -> AdminExecutionDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AdminExecutionResultDTO)
list_operations_executions_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< executeOperation reqDto
