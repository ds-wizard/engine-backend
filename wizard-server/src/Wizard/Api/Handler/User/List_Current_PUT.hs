module Wizard.Api.Handler.User.List_Current_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Profile.UserProfileService

type List_Current_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserProfileChangeDTO
    :> "users"
    :> "current"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_current_PUT
  :: Maybe String
  -> Maybe String
  -> UserProfileChangeDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserDTO)
list_current_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyUserProfile reqDto
