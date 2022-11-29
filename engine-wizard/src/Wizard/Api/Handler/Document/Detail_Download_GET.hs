module Wizard.Api.Handler.Document.Detail_Download_GET where

import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentService

type Detail_Download_GET =
  Header "Host" String
    :> "documents"
    :> Capture "docUuid" String
    :> "download"
    :> QueryParam "Authorization" String
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStream)

detail_download_GET
  :: Maybe String
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStream)
detail_download_GET mServerUrl docUuid mTokenHeader =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      (doc, result) <- downloadDocument docUuid
      let cdHeader = "attachment;filename=\"" ++ fromMaybe "export" doc.fileName ++ "\""
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
