module Registry.Api.Handler.Locale.Detail_Bundle_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Service.Locale.Bundle.LocaleBundleService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate

type Detail_Bundle_GET =
  Header "Authorization" String
    :> "locales"
    :> Capture "id" Coordinate
    :> "bundle"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)

detail_bundle_GET
  :: Maybe String
  -> Coordinate
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStreamLazy)
detail_bundle_GET mTokenHeader coordinate =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      zipFile <- exportBundle coordinate
      let cdHeader = "attachment;filename=\"locale.zip\""
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStreamLazy $ zipFile
