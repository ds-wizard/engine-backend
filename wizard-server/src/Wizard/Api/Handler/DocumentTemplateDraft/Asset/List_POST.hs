module Wizard.Api.Handler.DocumentTemplateDraft.Asset.List_POST where

import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateJM ()
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem DocumentTemplateAssetCreateDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "assets"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateAssetCreateDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateAssetDTO)
list_POST mTokenHeader mServerUrl reqDto tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        createAsset tmlId reqDto
