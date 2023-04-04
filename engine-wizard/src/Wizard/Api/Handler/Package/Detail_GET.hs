module Wizard.Api.Handler.Package.Detail_GET where

import Data.Maybe (fromMaybe)
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageDetailDTO
import Wizard.Api.Resource.Package.PackageDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "packages"
    :> Capture "pkgId" String
    :> QueryParam "excludeDeprecatedVersions" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageDetailDTO)

detail_GET
  :: Maybe String -> Maybe String -> String -> Maybe Bool -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageDetailDTO)
detail_GET mTokenHeader mServerUrl pkgId mExcludeDeprecatedVersions =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getPackageDetailById pkgId (fromMaybe False mExcludeDeprecatedVersions)
