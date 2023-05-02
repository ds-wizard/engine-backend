module Registry.Api.Handler.Package.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Package.PackageService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_GET =
  "packages"
    :> Capture "pkgId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageDetailDTO)

detail_GET :: String -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageDetailDTO)
detail_GET pkgId = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getPackageById pkgId
