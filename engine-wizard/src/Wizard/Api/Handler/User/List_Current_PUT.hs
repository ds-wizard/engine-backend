module Wizard.Api.Handler.User.List_Current_PUT where

import Control.Lens ((^.))
import qualified Data.UUID as U
import Servant

import LensesConfig
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileChangeJM ()
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserProfileJM ()
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserProfileService

type List_Current_PUT
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] UserProfileChangeDTO
     :> "users"
     :> "current"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] UserProfileDTO)

list_current_PUT ::
     Maybe String
  -> Maybe String
  -> UserProfileChangeDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] UserProfileDTO)
list_current_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
    addTraceUuidHeader =<< do
      user <- getCurrentUser
      modifyUserProfile (U.toString $ user ^. uuid) reqDto
