module Wizard.Api.Handler.User.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext

type List_Current_GET
   = Header "Authorization" String
     :> "users"
     :> "current"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] UserDTO)

list_current_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] UserDTO)
list_current_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService -> runInAuthService $ addTraceUuidHeader =<< getCurrentUser
