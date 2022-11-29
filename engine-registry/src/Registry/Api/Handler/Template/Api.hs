module Registry.Api.Handler.Template.Api where

import Servant

import Registry.Api.Handler.Template.Detail_Bundle_GET
import Registry.Api.Handler.Template.Detail_GET
import Registry.Api.Handler.Template.List_GET
import Registry.Model.Context.BaseContext

type TemplateAPI =
  List_GET
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

templateApi :: Proxy TemplateAPI
templateApi = Proxy

templateServer :: ServerT TemplateAPI BaseContextM
templateServer = list_GET :<|> detail_GET :<|> detail_bundle_GET
