module Wizard.Api.Handler.User.List_Consents_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Auth.AuthConsentDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO

type List_Consents_POST =
  Header "Host" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] AuthConsentDTO
    :> "users"
    :> "consents"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_consents_POST
  :: Maybe String
  -> Maybe String
  -> AuthConsentDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_consents_POST mServerUrl mUserAgent reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< confirmConsents reqDto Nothing
