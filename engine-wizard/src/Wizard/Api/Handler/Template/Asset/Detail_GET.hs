module Wizard.Api.Handler.Template.Asset.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
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
     :> Capture "assetUuid" U.UUID
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateAsset)

detail_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateAsset)
detail_GET mTokenHeader mServerUrl tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getTemplateAsset assetUuid
