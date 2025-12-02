module Wizard.Api.Handler.KnowledgeModelSecret.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret
import Wizard.Service.KnowledgeModel.Secret.KnowledgeModelSecretService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-secrets"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelSecret])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [KnowledgeModelSecret])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getKnowledgeModelSecrets
