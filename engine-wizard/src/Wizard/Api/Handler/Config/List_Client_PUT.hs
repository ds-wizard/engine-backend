module Wizard.Api.Handler.Config.List_Client_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Client_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AppConfigClientDTO
     :> "configs"
     :> "client"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigClientDTO)

list_client_PUT ::
     Maybe String -> AppConfigClientDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigClientDTO)
list_client_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyAppConfigClient reqDto
