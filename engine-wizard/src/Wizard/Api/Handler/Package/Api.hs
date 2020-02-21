module Wizard.Api.Handler.Package.Api where

import Servant

import Wizard.Api.Handler.Package.Detail_DELETE
import Wizard.Api.Handler.Package.Detail_GET
import Wizard.Api.Handler.Package.Detail_Pull_POST
import Wizard.Api.Handler.Package.List_DELETE
import Wizard.Api.Handler.Package.List_GET
import Wizard.Api.Handler.Package.List_POST
import Wizard.Model.Context.BaseContext

type PackageAPI
   = List_GET
     :<|> List_POST
     :<|> List_DELETE
     :<|> Detail_GET
     :<|> Detail_DELETE
     :<|> Detail_Pull_POST

packageApi :: Proxy PackageAPI
packageApi = Proxy

packageServer :: ServerT PackageAPI BaseContextM
packageServer = list_GET :<|> list_POST :<|> list_DELETE :<|> detail_GET :<|> detail_DELETE :<|> detail_pull_POST
