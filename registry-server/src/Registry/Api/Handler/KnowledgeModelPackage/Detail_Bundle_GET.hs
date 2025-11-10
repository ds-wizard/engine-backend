module Registry.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Registry.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_Bundle_GET =
  Header "Authorization" String
    :> "knowledge-model-packages"
    :> Capture "id" String
    :> "bundle"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)

detail_bundle_GET :: Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)
detail_bundle_GET mTokenHeader kmpId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< exportBundle kmpId
