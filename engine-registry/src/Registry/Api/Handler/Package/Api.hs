module Registry.Api.Handler.Package.Api where

import Servant

import Registry.Api.Handler.Package.Detail_Bundle_GET
import Registry.Api.Handler.Package.Detail_GET
import Registry.Api.Handler.Package.List_GET
import Registry.Model.Context.BaseContext

type PackageAPI =
  List_GET
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

packageApi :: Proxy PackageAPI
packageApi = Proxy

packageServer :: ServerT PackageAPI BaseContextM
packageServer = list_GET :<|> detail_GET :<|> detail_bundle_GET
