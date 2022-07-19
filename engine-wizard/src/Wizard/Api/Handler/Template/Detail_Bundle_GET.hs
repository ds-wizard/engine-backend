module Wizard.Api.Handler.Template.Detail_Bundle_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.TemplateBundle.TemplateBundleService

type Detail_Bundle_GET
   = Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> "bundle"
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)

detail_bundle_GET ::
     Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)
detail_bundle_GET mServerUrl tmlId =
  runInUnauthService mServerUrl NoTransaction $ do
    zipFile <- exportTemplateBundle tmlId
    let cdHeader = "attachment;filename=\"template.zip\""
    traceUuid <- asks _appContextTraceUuid
    return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStreamLazy $ zipFile
