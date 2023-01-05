module Wizard.Api.Handler.Questionnaire.Detail_Documents_Preview_GET where

import Control.Monad (msum)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant hiding (contentType)

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
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
    :> QueryParam "Authorization" String
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_documents_preview_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_documents_preview_GET mTokenHeader mServerUrl qtnUuid mTokenQueryHeader =
  getMaybeAuthServiceExecutor (msum [mTokenHeader, mTokenQueryHeader]) mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ do
      (doc, result) <- createDocumentPreviewForQtn qtnUuid
      case doc.state of
        DoneDocumentState -> do
          let cdHeader = fromMaybe "text/plain" doc.contentType
          traceUuid <- asks traceUuid
          return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
        ErrorDocumentState ->
          throwError $ SystemLogError (_ERROR_SERVICE_QTN__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW $ doc.workerLog)
        _ -> throwError AcceptedError
