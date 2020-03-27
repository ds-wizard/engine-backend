module Wizard.Api.Handler.Config.List_Auth_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Auth_GET
   = Header "Authorization" String
     :> "configs"
     :> "auth"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigAuthDTO)

list_auth_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigAuthDTO)
list_auth_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      getAppConfigAuth
