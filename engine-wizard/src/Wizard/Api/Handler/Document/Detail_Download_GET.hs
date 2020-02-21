module Wizard.Api.Handler.Document.Detail_Download_GET where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import LensesConfig
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type Detail_Download_GET
   = "documents"
     :> Capture "docUuid" String
     :> "download"
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] BS.ByteString)

detail_download_GET ::
     String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] BS.ByteString)
detail_download_GET docUuid =
  runInUnauthService $ do
    (doc, result) <- downloadDocument docUuid
    let cdHeader = "attachment;filename=" ++ fromMaybe "export" (doc ^. metadata . fileName)
    traceUuid <- asks _appContextTraceUuid
    return . addHeader (U.toString traceUuid) . addHeader cdHeader . BS.fromStrict $ result
