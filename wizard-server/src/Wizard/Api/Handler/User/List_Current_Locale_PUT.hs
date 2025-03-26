module Wizard.Api.Handler.User.List_Current_Locale_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Profile.UserProfileService
import WizardLib.Public.Api.Resource.User.UserLocaleDTO

type List_Current_Locale_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserLocaleDTO
    :> "users"
    :> "current"
    :> "locale"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserLocaleDTO)

list_current_locale_PUT :: Maybe String -> Maybe String -> UserLocaleDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserLocaleDTO)
list_current_locale_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< modifyLocale reqDto
