module Registry.Api.Handler.Package.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Package.PackageService
import Shared.Api.Handler.Common

type Detail_GET
   = "packages"
     :> Capture "pkgId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] PackageDetailDTO)

detail_GET :: String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] PackageDetailDTO)
detail_GET pkgId = runInUnauthService $ addTraceUuidHeader =<< getPackageById pkgId
