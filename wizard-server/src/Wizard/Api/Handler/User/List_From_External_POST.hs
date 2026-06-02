module Wizard.Api.Handler.User.List_From_External_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.RegistrationPending.UserRegistrationPendingService
import WizardLib.Public.Api.Resource.User.UserFromExternalDTO
import WizardLib.Public.Api.Resource.User.UserFromExternalJM ()
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO

type List_From_External_POST =
  Header "Host" String
    :> Header "Accept-Language" String
    :> Header "User-Agent" String
    :> ReqBody '[SafeJSON] UserFromExternalDTO
    :> "users"
    :> "from-external"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_from_external_POST
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> UserFromExternalDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_from_external_POST mServerUrl mAcceptLanguages mUserAgent reqDto =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< completeExternalRegistration reqDto mAcceptLanguages mUserAgent
