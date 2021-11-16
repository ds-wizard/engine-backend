module Wizard.Api.Handler.Config.List_App_Logo_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigLogoService

type List_App_Logo_DELETE
   = Header "Authorization" String
     :> "configs"
     :> "app"
     :> "logo"
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_app_logo_DELETE :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_app_logo_DELETE mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteLogo
      return NoContent
