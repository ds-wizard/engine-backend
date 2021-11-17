module Wizard.Api.Handler.Document.Detail_Download_GET where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import LensesConfig
import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type Detail_Download_GET
   = Header "Host" String
     :> "documents"
     :> Capture "docUuid" String
     :> "download"
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStream)

detail_download_GET ::
     Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStream)
detail_download_GET mServerUrl docUuid =
  runInUnauthService mServerUrl $ do
    (doc, result) <- downloadDocument docUuid
    let cdHeader = "attachment;filename=\"" ++ fromMaybe "export" (doc ^. fileName) ++ "\""
    traceUuid <- asks _appContextTraceUuid
    return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
