module Registry.Api.Handler.Locale.Detail_Bundle_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Service.Locale.Bundle.LocaleBundleService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_Bundle_GET =
  Header "Authorization" String
    :> "locales"
    :> Capture "documentTemplateId" String
    :> "bundle"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)

detail_bundle_GET
  :: Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)
detail_bundle_GET mTokenHeader tmlId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      zipFile <- exportBundle tmlId
      let cdHeader = "attachment;filename=\"locale.zip\""
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStreamLazy $ zipFile
