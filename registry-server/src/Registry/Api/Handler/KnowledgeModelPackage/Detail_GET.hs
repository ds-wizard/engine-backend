module Registry.Api.Handler.KnowledgeModelPackage.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_GET =
  "knowledge-model-packages"
    :> Capture "id" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)

detail_GET :: String -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)
detail_GET kmpId = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getPackageById kmpId
