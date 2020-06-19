module Wizard.Api.Handler.Template.Api where

import Servant

import Wizard.Api.Handler.Template.Asset.Api
import Wizard.Api.Handler.Template.Detail_DELETE
import Wizard.Api.Handler.Template.Detail_GET
import Wizard.Api.Handler.Template.Detail_PUT
import Wizard.Api.Handler.Template.Detail_Pull_POST
import Wizard.Api.Handler.Template.File.Api
import Wizard.Api.Handler.Template.List_GET
import Wizard.Api.Handler.Template.List_POST
import Wizard.Model.Context.BaseContext

type TemplateAPI
   = TemplateAssetAPI
     :<|> TemplateFileAPI
     :<|> List_GET
     :<|> List_POST
     :<|> Detail_GET
     :<|> Detail_PUT
     :<|> Detail_DELETE
     :<|> Detail_Pull_POST

templateApi :: Proxy TemplateAPI
templateApi = Proxy

templateServer :: ServerT TemplateAPI BaseContextM
templateServer =
  templateAssetServer :<|> templateFileServer :<|> list_GET :<|> list_POST :<|> detail_GET :<|> detail_PUT :<|>
  detail_DELETE :<|>
  detail_pull_POST
