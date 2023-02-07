module Wizard.Api.Handler.DocumentTemplateDraft.Asset.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_GET
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_PUT
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_DELETE
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_GET
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_PUT
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.List_GET
import Wizard.Api.Handler.DocumentTemplateDraft.Asset.List_POST
import Wizard.Model.Context.BaseContext

type DocumentTemplateAssetAPI =
  Tags "Document Template Draft Asset"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_DELETE
          :<|> Detail_Content_GET
          :<|> Detail_Content_PUT
       )

documentTemplateAssetApi :: Proxy DocumentTemplateAssetAPI
documentTemplateAssetApi = Proxy

documentTemplateAssetServer :: ServerT DocumentTemplateAssetAPI BaseContextM
documentTemplateAssetServer = list_GET :<|> list_POST :<|> detail_GET :<|> detail_PUT :<|> detail_DELETE :<|> detail_content_GET :<|> detail_content_PUT
