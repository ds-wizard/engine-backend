module Wizard.Api.Handler.Registry.List_Signup_POST where

import Servant

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Api.Resource.Registry.RegistryCreateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Registry.RegistryService

type List_Signup_POST
   = ReqBody '[ SafeJSON] RegistryCreateDTO
     :> "registry"
     :> "signup"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_signup_POST :: RegistryCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_signup_POST reqDto = runInUnauthService $ addTraceUuidHeader =<< signUpToRegistry reqDto
