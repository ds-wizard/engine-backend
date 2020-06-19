module Wizard.Api.Handler.Template.Asset.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type List_GET
   = Header "Authorization" String
     :> "templates"
     :> Capture "tmlId" String
     :> "assets"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [TemplateAsset])

list_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [TemplateAsset])
list_GET mTokenHeader tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateAssets tmlId
