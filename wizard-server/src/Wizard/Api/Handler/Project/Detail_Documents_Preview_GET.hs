module Wizard.Api.Handler.Project.Detail_Documents_Preview_GET where

import qualified Data.UUID as U
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
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "documents"
    :> "preview"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_documents_preview_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_documents_preview_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ do
      (doc, fileDto) <- createDocumentPreviewForProject uuid
      case doc.state of
        DoneDocumentState -> addTraceUuidHeader fileDto
        ErrorDocumentState ->
          throwError $ SystemLogError (_ERROR_SERVICE_PROJECT__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW $ doc.workerLog)
        _ -> throwError AcceptedError
