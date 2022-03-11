module Wizard.Api.Handler.Template.Asset.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type Detail_DELETE
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> "assets"
     :> Capture "assetUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE ::
     Maybe String
  -> Maybe String
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteTemplateAsset tmlId assetUuid
      return NoContent
