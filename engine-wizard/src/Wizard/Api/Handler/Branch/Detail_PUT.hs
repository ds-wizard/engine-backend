module Wizard.Api.Handler.Branch.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchChangeJM ()
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] BranchChangeDTO
    :> "branches"
    :> Capture "bUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] BranchDetailDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> BranchChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] BranchDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyBranch bUuid reqDto
