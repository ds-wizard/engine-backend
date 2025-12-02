module Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_PUT where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_Content_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem DocumentTemplateAssetCreateDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> "content"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)

detail_content_PUT
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateAssetCreateDTO
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateAsset)
detail_content_PUT mTokenHeader mServerUrl reqDto tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        modifyAssetContent assetUuid reqDto
