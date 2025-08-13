module Wizard.Api.Handler.TypeHint.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.TypeHint.TypeHintIJM ()
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintRequestJM ()
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.TypeHint.TypeHintService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TypeHintRequestDTO
    :> "type-hints"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TypeHintIDTO])

list_POST
  :: Maybe String
  -> Maybe String
  -> TypeHintRequestDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TypeHintIDTO])
list_POST mTokenHeader mServerUrl reqDto@(QuestionnaireTypeHintRequest' _) =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTypeHints reqDto
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTypeHints reqDto
