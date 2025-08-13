module Wizard.Api.Handler.TypeHint.Test_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.TypeHint.TypeHintIJM ()
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.TypeHint.TypeHintService
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

type Test_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TypeHintTestRequestDTO
    :> "type-hints"
    :> "test"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TypeHintExchange)

test_POST
  :: Maybe String
  -> Maybe String
  -> TypeHintTestRequestDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TypeHintExchange)
test_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< testTypeHints reqDto
