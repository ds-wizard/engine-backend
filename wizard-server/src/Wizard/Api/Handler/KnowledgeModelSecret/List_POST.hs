module Wizard.Api.Handler.KnowledgeModelSecret.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeDTO
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeJM ()
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret
import Wizard.Service.KnowledgeModel.Secret.KnowledgeModelSecretService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelSecretChangeDTO
    :> "knowledge-model-secrets"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelSecret)

list_POST
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelSecretChangeDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelSecret)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createKnowledgeModelSecret reqDto
