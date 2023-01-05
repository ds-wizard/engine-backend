module Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)
detail_GET mTokenHeader mServerUrl tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getAsset assetUuid
