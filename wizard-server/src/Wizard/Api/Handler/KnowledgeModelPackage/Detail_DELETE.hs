module Wizard.Api.Handler.KnowledgeModelPackage.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl pkgUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deletePackage pkgUuid
        return NoContent
