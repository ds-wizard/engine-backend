module Wizard.Api.Handler.App.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppChangeJM ()
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Model.App.App
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] AppChangeDTO
    :> "apps"
    :> Capture "aUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] App)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> AppChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] App)
detail_PUT mTokenHeader mServerUrl reqDto aUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyApp aUuid reqDto
