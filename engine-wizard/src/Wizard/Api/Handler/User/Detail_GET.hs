module Wizard.Api.Handler.User.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type Detail_GET
   = Header "Authorization" String
     :> "users"
     :> Capture "uUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] UserDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] UserDTO)
detail_GET mTokenHeader uUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getUserByIdDto uUuid
