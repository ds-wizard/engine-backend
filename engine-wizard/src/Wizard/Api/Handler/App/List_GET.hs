module Wizard.Api.Handler.App.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppService

type List_GET
   = Header "Host" String
     :> "apps"
     :> QueryParam "appId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [AppDTO])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [AppDTO])
list_GET mServerUrl mAppId = runInUnauthService mServerUrl $ addTraceUuidHeader =<< getApps mAppId
