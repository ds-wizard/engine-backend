module Wizard.Api.Handler.Registry.List_Signup_POST where

import Servant

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Api.Resource.Registry.RegistryCreateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Registry.Registration.RegistryRegistrationService

type List_Signup_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] RegistryCreateDTO
    :> "registry"
    :> "signup"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

list_signup_POST
  :: Maybe String -> RegistryCreateDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
list_signup_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< signUpToRegistry reqDto
