module RegistryLib.Api.Handler.Organization.List_Simple_GET where

import Servant

import Shared.Common.Api.Handler.Common
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleJM ()

type List_Simple_GET =
  "organizations"
    :> "simple"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OrganizationSimpleDTO])

list_simple_GET_Api :: Proxy List_Simple_GET
list_simple_GET_Api = Proxy
