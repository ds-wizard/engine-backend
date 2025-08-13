module Wizard.Api.Handler.TypeHint.Legacy_POST where

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

type Legacy_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TypeHintLegacyRequestDTO
    :> "type-hints-legacy"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TypeHintLegacyIDTO])

legacy_POST
  :: Maybe String
  -> Maybe String
  -> TypeHintLegacyRequestDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TypeHintLegacyIDTO])
legacy_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< getLegacyTypeHints reqDto
