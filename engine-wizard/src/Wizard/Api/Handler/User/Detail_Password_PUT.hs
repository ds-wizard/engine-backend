module Wizard.Api.Handler.User.Detail_Password_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserPasswordJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type Detail_Password_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] UserPasswordDTO
     :> "users"
     :> Capture "uUuid" String
     :> "password"
     :> QueryParam "hash" String
     :> Verb PUT 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_password_PUT ::
     Maybe String
  -> UserPasswordDTO
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_password_PUT mTokenHeader reqDto uUuid mHash =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      ia <- isAdmin
      if ia
        then do
          changeUserPasswordByAdmin uUuid reqDto
          return NoContent
        else do
          changeUserPasswordByHash uUuid mHash reqDto
          return NoContent
