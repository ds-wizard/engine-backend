module Wizard.Api.Handler.Package.Detail_Bundle_GET where

import Control.Monad.Reader (asks)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.PackageBundle.PackageBundleJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.PackageBundle.PackageBundleService

type Detail_Bundle_GET
   = Header "Host" String
     :> "packages"
     :> Capture "pkgId" String
     :> "bundle"
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStream)

detail_bundle_GET ::
     Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] FileStream)
detail_bundle_GET mServerUrl pkgId =
  runInUnauthService mServerUrl $ do
    dto <- exportPackageBundle pkgId
    let result = encode dto
    let cdHeader = "attachment;filename=" ++ pkgId ++ ".km"
    traceUuid <- asks _appContextTraceUuid
    return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream . BSL.toStrict $ result
