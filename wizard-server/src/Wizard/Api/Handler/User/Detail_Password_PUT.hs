module Wizard.Api.Handler.User.Detail_Password_PUT where

import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserPasswordJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type Detail_Password_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserPasswordDTO
    :> "users"
    :> Capture "uUuid" U.UUID
    :> "password"
    :> QueryParam "hash" String
    :> Verb PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_password_PUT
  :: Maybe String
  -> Maybe String
  -> UserPasswordDTO
  -> U.UUID
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_password_PUT mTokenHeader mServerUrl reqDto uUuid mHash =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        ia <- isAdmin
        if ia
          then do
            changeUserPasswordByAdmin uUuid reqDto
            return NoContent
          else do
            let hash = fromMaybe (U.toString U.nil) mHash
            changeUserPasswordByHash uUuid hash reqDto
            return NoContent
