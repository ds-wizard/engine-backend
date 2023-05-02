module Wizard.Api.Handler.Package.Detail_DELETE where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "packages"
    :> Capture "pkgId" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl pkgId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deletePackage pkgId
        return NoContent
