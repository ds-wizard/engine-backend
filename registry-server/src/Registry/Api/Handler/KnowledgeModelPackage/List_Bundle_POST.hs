module Registry.Api.Handler.KnowledgeModelPackage.List_Bundle_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

list_bundle_POST
  :: Maybe String
  -> KnowledgeModelBundle
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importBundle reqDto
