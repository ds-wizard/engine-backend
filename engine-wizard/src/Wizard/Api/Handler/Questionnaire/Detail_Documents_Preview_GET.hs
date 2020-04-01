module Wizard.Api.Handler.Questionnaire.Detail_Documents_Preview_GET where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant hiding (contentType)

import LensesConfig
import Shared.Api.Handler.Common
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentService

type Detail_Documents_Preview_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "documents"
     :> "preview"
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_documents_preview_GET ::
     Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_documents_preview_GET mTokenHeader qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ do
      checkPermission mTokenHeader "QTN_PERM"
      (doc, result) <- createPreview qtnUuid
      case doc ^. state of
        DoneDocumentState -> do
          let cdHeader = fromMaybe "text/plain" (doc ^. metadata . contentType)
          traceUuid <- asks _appContextTraceUuid
          return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
        _ -> throwError AcceptedError
