module Wizard.Api.Handler.User.List_Current_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserProfileService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserProfileDTO)

list_current_GET
  :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserProfileDTO)
list_current_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        getUserProfile (U.toString user.uuid)
