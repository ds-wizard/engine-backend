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
     :> "packages"
     :> Capture "pkgId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] PackageDetailDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] PackageDetailDTO)
detail_GET mTokenHeader pkgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "PM_READ_PERM"
      getPackageById pkgId
