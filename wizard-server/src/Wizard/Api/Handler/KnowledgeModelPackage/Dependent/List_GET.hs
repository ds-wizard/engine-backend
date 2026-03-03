module Wizard.Api.Handler.KnowledgeModelPackage.Dependent.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "dependents"
    :> QueryParam "allVersions" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageDeletionImpact])

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe Bool
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageDeletionImpact])
list_GET mTokenHeader mServerUrl uuid mAllVersions =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getDependentPackageResources uuid mAllVersions
