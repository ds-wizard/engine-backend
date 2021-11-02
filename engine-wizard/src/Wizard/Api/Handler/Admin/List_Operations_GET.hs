module Wizard.Api.Handler.Admin.List_Operations_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Admin.AdminJM ()
import Wizard.Model.Admin.Admin
import Wizard.Model.Context.BaseContext
import Wizard.Service.Admin.AdminService

type List_Operations_GET
   = Header "Authorization" String
     :> "admin"
     :> "operations"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [AdminSection])

list_operations_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [AdminSection])
list_operations_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getAdminOperations
