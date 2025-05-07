module Wizard.Api.Handler.User.List_Current_Locale_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Profile.UserProfileService
import WizardLib.Public.Api.Resource.User.UserLocaleDTO

type List_Current_Locale_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "locale"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserLocaleDTO)

list_current_locale_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserLocaleDTO)
list_current_locale_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getLocale
