module Wizard.Api.Handler.Version.Detail_Version_Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Api.Resource.Version.VersionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Version.VersionService

type Detail_Version_Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] VersionDTO
    :> "branches"
    :> Capture "bUuid" String
    :> "versions"
    :> Capture "version" String
    :> Verb 'PUT 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageSimpleDTO)

detail_version_detail_PUT
  :: Maybe String
  -> Maybe String
  -> VersionDTO
  -> String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageSimpleDTO)
detail_version_detail_PUT mTokenHeader mServerUrl reqDto bUuid version =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< publishPackage bUuid version reqDto
