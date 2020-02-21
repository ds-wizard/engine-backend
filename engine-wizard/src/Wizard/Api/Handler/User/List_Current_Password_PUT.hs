module Wizard.Api.Handler.User.List_Current_Password_PUT where

import Control.Lens ((^.))
import qualified Data.UUID as U
import Servant

import LensesConfig
import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserPasswordJM ()
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_Current_Password_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] UserPasswordDTO
     :> "users"
     :> "current"
     :> "password"
     :> Verb PUT 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_current_password_PUT ::
     Maybe String -> UserPasswordDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_current_password_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      user <- getCurrentUser
      changeCurrentUserPassword (U.toString $ user ^. uuid) reqDto
      return NoContent
