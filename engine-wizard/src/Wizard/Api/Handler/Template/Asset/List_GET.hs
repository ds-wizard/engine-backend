module Wizard.Api.Handler.Template.Asset.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "templates"
    :> Capture "templateId" String
    :> "assets"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TemplateAsset])

list_GET
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TemplateAsset])
list_GET mTokenHeader mServerUrl tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getTemplateAssets tmlId
