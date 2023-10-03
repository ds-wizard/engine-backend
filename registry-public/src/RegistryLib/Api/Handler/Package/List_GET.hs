module RegistryLib.Api.Handler.Package.List_GET where

import Servant

import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import RegistryLib.Api.Resource.Package.PackageSimpleJM ()
import Shared.Common.Api.Handler.Common

type List_GET =
  Header "Authorization" String
    :> Header "x-user-count" String
    :> Header "x-pkg-count" String
    :> Header "x-qtn-count" String
    :> Header "x-branch-count" String
    :> Header "x-doc-count" String
    :> Header "x-tml-count" String
    :> "packages"
    :> QueryParam "organizationId" String
    :> QueryParam "kmId" String
    :> QueryParam "metamodelVersion" Int
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [PackageSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy
