module Wizard.Api.Handler.Template.File.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Template.File.Detail_DELETE
import Wizard.Api.Handler.Template.File.Detail_GET
import Wizard.Api.Handler.Template.File.Detail_PUT
import Wizard.Api.Handler.Template.File.List_GET
import Wizard.Api.Handler.Template.File.List_POST
import Wizard.Model.Context.BaseContext

type TemplateFileAPI =
  Tags "Template File"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_DELETE
       )

templateFileApi :: Proxy TemplateFileAPI
templateFileApi = Proxy

templateFileServer :: ServerT TemplateFileAPI BaseContextM
templateFileServer = list_GET :<|> list_POST :<|> detail_GET :<|> detail_PUT :<|> detail_DELETE
