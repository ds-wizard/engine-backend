module Wizard.Api.Handler.Registry.List_Confirmation_POST where

import Servant

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Registry.RegistryService

type List_Confirmation_POST =
  Header "Host" String
    :> ReqBody '[SafeJSON] RegistryConfirmationDTO
    :> "registry"
    :> "confirmation"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

list_confirmation_POST
  :: Maybe String -> RegistryConfirmationDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
list_confirmation_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< confirmRegistration reqDto
