module Wizard.Api.Handler.Branch.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type Detail_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "branches"
     :> Capture "bUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] BranchDetailDTO)

detail_GET ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] BranchDetailDTO)
detail_GET mTokenHeader mServerUrl bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getBranchById bUuid
