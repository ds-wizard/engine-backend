module Wizard.Api.Handler.DocumentTemplate.Detail_Bundle_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService

type Detail_Bundle_GET =
  Header "Host" String
    :> "document-templates"
    :> Capture "documentTemplateId" String
    :> "bundle"
    :> QueryParam "Authorization" String
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)

detail_bundle_GET
  :: Maybe String
  -> String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)
detail_bundle_GET mServerUrl tmlId mTokenHeader =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      zipFile <- exportBundle tmlId
      let cdHeader = "attachment;filename=\"template.zip\""
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStreamLazy $ zipFile
