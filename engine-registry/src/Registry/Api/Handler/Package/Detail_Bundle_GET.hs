module Registry.Api.Handler.Package.Detail_Bundle_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.PackageBundle.PackageBundleService
import Shared.Api.Handler.Common

type Detail_Bundle_GET
   = Header "Authorization" String
     :> "packages"
     :> Capture "pkgId" String
     :> "bundle"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] PackageBundleDTO)

detail_bundle_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] PackageBundleDTO)
detail_bundle_GET mTokenHeader pkgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getPackageBundle pkgId
