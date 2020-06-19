module Wizard.Api.Handler.Template.Asset.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type Detail_GET
   = Header "Authorization" String
     :> "templates"
     :> Capture "tmlId" String
     :> "assets"
     :> Capture "assetUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateAsset)

detail_GET :: Maybe String -> String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateAsset)
detail_GET mTokenHeader tmlId assetUuid =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateAsset tmlId assetUuid
