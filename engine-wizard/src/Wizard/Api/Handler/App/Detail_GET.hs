module Wizard.Api.Handler.App.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.App.AppDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppService

type Detail_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "apps"
     :> Capture "aUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppDetailDTO)

detail_GET ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppDetailDTO)
detail_GET mTokenHeader mServerUrl aUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getAppById aUuid
