module Wizard.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type Detail_Documents_Preview_Settings_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateDraftDataChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "documents"
    :> "preview"
    :> "settings"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDataDTO)

detail_documents_preview_settings_PUT
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateDraftDataChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDataDTO)
detail_documents_preview_settings_PUT mTokenHeader mServerUrl reqDto tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyDraftData tmlId reqDto
