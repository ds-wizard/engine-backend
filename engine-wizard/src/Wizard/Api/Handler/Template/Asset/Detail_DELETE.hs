module Wizard.Api.Handler.Template.Asset.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type Detail_DELETE
   = Header "Authorization" String
     :> "templates"
     :> Capture "tmlId" String
     :> "assets"
     :> Capture "assetUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader tmlId assetUuid =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteTemplateAsset tmlId assetUuid
      return NoContent
