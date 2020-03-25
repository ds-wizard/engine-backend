module Wizard.Api.Handler.Config.List_Info_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Info_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AppConfigInfoDTO
     :> "configs"
     :> "info"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigInfoDTO)

list_info_PUT ::
     Maybe String -> AppConfigInfoDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigInfoDTO)
list_info_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyAppConfigInfo reqDto
