module Wizard.Api.Handler.Locale.Detail_Bundle_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.Bundle.LocaleBundleService
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()

type Detail_Bundle_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "uuid" U.UUID
    :> "bundle"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_bundle_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_bundle_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTemporaryFileWithBundle uuid
