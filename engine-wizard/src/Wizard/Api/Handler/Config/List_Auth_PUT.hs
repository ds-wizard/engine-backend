module Wizard.Api.Handler.Config.List_Auth_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Auth_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AppConfigAuthDTO
     :> "configs"
     :> "auth"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigAuthDTO)

list_auth_PUT ::
     Maybe String -> AppConfigAuthDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigAuthDTO)
list_auth_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyAppConfigAuth reqDto
