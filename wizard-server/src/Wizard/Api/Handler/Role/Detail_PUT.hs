module Wizard.Api.Handler.Role.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Role.RoleService
import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Api.Resource.User.RoleChangeJM ()
import WizardLib.Public.Api.Resource.User.RoleListJM ()
import WizardLib.Public.Model.User.RoleList

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] RoleChangeDTO
    :> "roles"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] RoleList)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> RoleChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] RoleList)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyRole uuid reqDto
