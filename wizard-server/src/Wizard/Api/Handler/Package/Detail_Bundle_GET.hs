module Wizard.Api.Handler.Package.Detail_Bundle_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.Bundle.PackageBundleService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()

type Detail_Bundle_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "packages"
    :> Capture "pkgId" String
    :> "bundle"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_bundle_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_bundle_GET mTokenHeader mServerUrl pkgId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTemporaryFileWithBundle pkgId
