module Wizard.Api.Handler.Package.Detail_Pull_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.PackageBundle.PackageBundleService

type Detail_Pull_POST
   = Header "Authorization" String
     :> "packages"
     :> Capture "pkgId" String
     :> "pull"
     :> Verb POST 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_pull_POST :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_pull_POST mTokenHeader pkgId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "PM_WRITE_PERM"
      pullPackageBundleFromRegistry pkgId
      return NoContent
