module Wizard.Api.Handler.Branch.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type Detail_DELETE
   = Header "Authorization" String
     :> "branches"
     :> Capture "bUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader bUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "KM_PERM"
      deleteBranch bUuid
      return NoContent
