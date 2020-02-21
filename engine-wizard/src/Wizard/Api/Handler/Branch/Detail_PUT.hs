module Wizard.Api.Handler.Branch.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchChangeJM ()
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] BranchChangeDTO
     :> "branches"
     :> Capture "bUuid" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] BranchDetailDTO)

detail_PUT ::
     Maybe String
  -> BranchChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] BranchDetailDTO)
detail_PUT mTokenHeader reqDto bUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "KM_PERM"
      modifyBranch bUuid reqDto
