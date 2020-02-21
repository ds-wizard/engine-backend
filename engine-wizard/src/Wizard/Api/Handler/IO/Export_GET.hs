module Wizard.Api.Handler.IO.Export_GET where

import Control.Monad.Reader (asks)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.UUID as U
import Servant

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.PackageBundle.PackageBundleJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.PackageBundle.PackageBundleService

type Export_GET
   = "export"
     :> Capture "pkgId" String
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] BS.ByteString)

export_GET ::
     String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Disposition" String] BS.ByteString)
export_GET pkgId =
  runInUnauthService $ do
    dto <- exportPackageBundle pkgId
    let result = encode dto
    let cdHeader = "attachment;filename=" ++ pkgId ++ ".km"
    traceUuid <- asks _appContextTraceUuid
    return . addHeader (U.toString traceUuid) . addHeader cdHeader $ result
