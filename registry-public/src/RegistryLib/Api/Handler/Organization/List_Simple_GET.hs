module RegistryLib.Api.Handler.Organization.List_Simple_GET where

import Servant

import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Api.Handler.Common

type List_Simple_GET =
  "organizations"
    :> "simple"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OrganizationSimple])

list_simple_GET_Api :: Proxy List_Simple_GET
list_simple_GET_Api = Proxy
