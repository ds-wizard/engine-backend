module Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateAssetChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateAssetChangeDTO
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)
detail_PUT mTokenHeader mServerUrl reqDto tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyAsset assetUuid reqDto
