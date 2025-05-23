module Wizard.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()

type Detail_Documents_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "documents"
    :> "preview"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_documents_preview_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_documents_preview_GET mTokenHeader mServerUrl documentTemplateId =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ do
      (doc, fileDto) <- createDocumentPreviewForDocTmlDraft documentTemplateId
      case doc.state of
        DoneDocumentState -> addTraceUuidHeader fileDto
        ErrorDocumentState ->
          throwError $ SystemLogError (_ERROR_SERVICE_QTN__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW $ doc.workerLog)
        _ -> throwError AcceptedError
