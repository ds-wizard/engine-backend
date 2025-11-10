module Wizard.Api.Handler.KnowledgeModelSecret.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM ()
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret
import Wizard.Service.KnowledgeModel.Secret.KnowledgeModelSecretService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelSecretChangeDTO
    :> "knowledge-model-secrets"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelSecret)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelSecretChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelSecret)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyKnowledgeModelSecret uuid reqDto
