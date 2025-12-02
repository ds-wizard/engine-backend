module Wizard.Api.Handler.KnowledgeModelPackage.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelPackageChangeDTO
    :> "knowledge-model-packages"
    :> Capture "id" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageChangeDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelPackageChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageChangeDTO)
detail_PUT mTokenHeader mServerUrl reqDto pkgId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPackage pkgId reqDto
