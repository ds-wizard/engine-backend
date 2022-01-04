module Wizard.Api.Handler.Template.Asset.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Template.Asset.Detail_Content_GET
import Wizard.Api.Handler.Template.Asset.Detail_DELETE
import Wizard.Api.Handler.Template.Asset.Detail_GET
import Wizard.Api.Handler.Template.Asset.List_GET
import Wizard.Api.Handler.Template.Asset.List_POST
import Wizard.Model.Context.BaseContext

type TemplateAssetAPI
   = Tags "Template Asset"
     :> (List_GET
         :<|> List_POST
         :<|> Detail_GET
         :<|> Detail_DELETE
         :<|> Detail_Content_GET)

templateAssetApi :: Proxy TemplateAssetAPI
templateAssetApi = Proxy

templateAssetServer :: ServerT TemplateAssetAPI BaseContextM
templateAssetServer = list_GET :<|> list_POST :<|> detail_GET :<|> detail_DELETE :<|> detail_content_GET
