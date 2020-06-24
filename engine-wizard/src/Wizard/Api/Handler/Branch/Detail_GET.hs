module Wizard.Api.Handler.Branch.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type Detail_GET
   = Header "Authorization" String
     :> "branches"
     :> Capture "bUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] BranchDetailDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] BranchDetailDTO)
detail_GET mTokenHeader bUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getBranchById bUuid
