module Wizard.Api.Handler.Registry.List_Confirmation_POST where

import Servant

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Registry.RegistryService

type List_Confirmation_POST
   = ReqBody '[ SafeJSON] RegistryConfirmationDTO
     :> "registry"
     :> "confirmation"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_confirmation_POST ::
     RegistryConfirmationDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_confirmation_POST reqDto = runInUnauthService $ addTraceUuidHeader =<< confirmRegistration reqDto
