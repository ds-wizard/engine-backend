module Wizard.Api.Handler.Branch.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type List_GET
   = Header "Authorization" String
     :> "branches"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [BranchDTO])

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [BranchDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "KM_PERM"
      getBranches
