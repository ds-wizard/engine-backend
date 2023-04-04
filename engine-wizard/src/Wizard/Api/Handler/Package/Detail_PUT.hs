module Wizard.Api.Handler.Package.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageChangeDTO
import Wizard.Api.Resource.Package.PackageChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PackageChangeDTO
    :> "packages"
    :> Capture "pkgId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageChangeDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> PackageChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageChangeDTO)
detail_PUT mTokenHeader mServerUrl reqDto pkgId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPackage pkgId reqDto
