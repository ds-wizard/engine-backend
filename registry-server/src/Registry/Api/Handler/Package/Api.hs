module Registry.Api.Handler.Package.Api where

import Servant

import Registry.Api.Handler.Package.Detail_Bundle_GET
import Registry.Api.Handler.Package.Detail_GET
import Registry.Api.Handler.Package.List_Bundle_POST
import Registry.Api.Handler.Package.List_GET
import Registry.Model.Context.BaseContext
import RegistryLib.Api.Handler.Package.List_Bundle_POST
import RegistryLib.Api.Handler.Package.List_GET

type PackageAPI =
  List_GET
    :<|> List_Bundle_POST
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

packageApi :: Proxy PackageAPI
packageApi = Proxy

packageServer :: ServerT PackageAPI BaseContextM
packageServer = list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET
