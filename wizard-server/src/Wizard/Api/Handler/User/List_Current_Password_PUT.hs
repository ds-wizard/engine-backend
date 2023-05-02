module Wizard.Api.Handler.User.List_Current_Password_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserPasswordJM ()
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserProfileService

type List_Current_Password_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserPasswordDTO
    :> "users"
    :> "current"
    :> "password"
    :> Verb PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_password_PUT
  :: Maybe String
  -> Maybe String
  -> UserPasswordDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_password_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        changeUserProfilePassword user.uuid reqDto
        return NoContent
