module Wizard.Api.Handler.User.Detail_State_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type Detail_State_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserStateDTO
    :> "users"
    :> Capture "uUuid" String
    :> "state"
    :> QueryParam "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserStateDTO)

detail_state_PUT
  :: Maybe String
  -> Maybe String
  -> UserStateDTO
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserStateDTO)
detail_state_PUT mTokenHeader mServerUrl reqDto uUuid mHash =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< changeUserState uUuid mHash reqDto
