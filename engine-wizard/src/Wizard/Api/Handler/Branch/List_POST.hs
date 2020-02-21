module Wizard.Api.Handler.Branch.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchCreateJM ()
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] BranchCreateDTO
     :> "branches"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] BranchDTO)

list_POST :: Maybe String -> BranchCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] BranchDTO)
list_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "KM_PERM"
      createBranch reqDto
