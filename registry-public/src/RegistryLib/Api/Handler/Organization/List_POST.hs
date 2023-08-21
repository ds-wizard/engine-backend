module RegistryLib.Api.Handler.Organization.List_POST where

import Servant

import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import Shared.Common.Api.Handler.Common

type List_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] OrganizationCreateDTO
    :> "organizations"
    :> QueryParam "callback" String
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

list_POST_Api :: Proxy List_POST
list_POST_Api = Proxy
