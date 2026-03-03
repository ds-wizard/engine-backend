module Wizard.Api.Handler.KnowledgeModelPackage.Detail_GET where

import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> QueryParam "excludeDeprecatedVersions" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)

detail_GET :: Maybe String -> Maybe String -> U.UUID -> Maybe Bool -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)
detail_GET mTokenHeader mServerUrl pkgUuid mExcludeDeprecatedVersions =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getPackageDetailByUuid pkgUuid (fromMaybe False mExcludeDeprecatedVersions)
