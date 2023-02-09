module Wizard.Api.Handler.Questionnaire.Detail_Documents_Preview_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentService

type Detail_Documents_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" String
    :> "documents"
    :> "preview"
    :> Verb GET 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_documents_preview_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_documents_preview_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ do
      (doc, link) <- createDocumentPreviewForQtn qtnUuid
      case doc.state of
        DoneDocumentState ->
          throwError $ FoundError link
        ErrorDocumentState ->
          throwError $ SystemLogError (_ERROR_SERVICE_QTN__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW $ doc.workerLog)
        _ -> throwError AcceptedError
