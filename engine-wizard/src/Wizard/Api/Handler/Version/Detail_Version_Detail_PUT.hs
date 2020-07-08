module Wizard.Api.Handler.Version.Detail_Version_Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Version.VersionDTO
import Wizard.Api.Resource.Version.VersionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Version.VersionService

type Detail_Version_Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] VersionDTO
     :> "branches"
     :> Capture "bUuid" String
     :> "versions"
     :> Capture "version" String
     :> Verb 'PUT 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] PackageSimpleDTO)

detail_version_detail_PUT ::
     Maybe String
  -> VersionDTO
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] PackageSimpleDTO)
detail_version_detail_PUT mTokenHeader reqDto bUuid version =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< publishPackage bUuid version reqDto
