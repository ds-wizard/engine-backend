module Wizard.Api.Handler.Package.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type Detail_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "packages"
     :> Capture "pkgId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] PackageDetailDTO)

detail_GET ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] PackageDetailDTO)
detail_GET mTokenHeader mServerUrl pkgId =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService $ addTraceUuidHeader =<< getPackageDetailById pkgId
