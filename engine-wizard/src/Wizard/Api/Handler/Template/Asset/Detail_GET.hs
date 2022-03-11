module Wizard.Api.Handler.Template.Asset.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type Detail_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> "assets"
     :> Capture "assetUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateAsset)

detail_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateAsset)
detail_GET mTokenHeader mServerUrl tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateAsset assetUuid
