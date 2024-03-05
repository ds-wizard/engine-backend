module Wizard.Api.Handler.AppKey.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Context.BaseContext
import WizardLib.Public.Service.UserToken.UserTokenService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "app-keys"
    :> Capture "uuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteTokenByUuid uuid
        return NoContent
