module Wizard.Api.Handler.User.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserChangeJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] UserChangeDTO
     :> "users"
     :> Capture "uUuid" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] UserDTO)

detail_PUT :: Maybe String -> UserChangeDTO -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] UserDTO)
detail_PUT mTokenHeader reqDto uUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "UM_PERM"
      modifyUser uUuid reqDto
