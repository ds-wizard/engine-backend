module Wizard.Api.Handler.Typehint.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Typehint.TypehintIJM ()
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Api.Resource.Typehint.TypehintRequestJM ()
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.Typehint.TypehintService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TypehintRequestDTO
    :> "typehints"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TypehintIDTO])

list_POST
  :: Maybe String
  -> Maybe String
  -> TypehintRequestDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TypehintIDTO])
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ addTraceUuidHeader =<< getTypehints reqDto
