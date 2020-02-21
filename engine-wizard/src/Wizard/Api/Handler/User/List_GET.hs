module Wizard.Api.Handler.User.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_GET
   = Header "Authorization" String
     :> "users"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [UserDTO])

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [UserDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "UM_PERM"
      getUsers
