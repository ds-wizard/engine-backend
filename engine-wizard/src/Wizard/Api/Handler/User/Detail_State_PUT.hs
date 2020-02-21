module Wizard.Api.Handler.User.Detail_State_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type Detail_State_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] UserStateDTO
     :> "users"
     :> Capture "uUuid" String
     :> "state"
     :> QueryParam "hash" String
     :> Verb PUT 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] UserStateDTO)

detail_state_PUT ::
     Maybe String
  -> UserStateDTO
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] UserStateDTO)
detail_state_PUT mTokenHeader reqDto uUuid mHash =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< changeUserState uUuid mHash reqDto
