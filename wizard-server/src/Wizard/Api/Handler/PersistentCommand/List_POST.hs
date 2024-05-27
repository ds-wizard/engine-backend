module Wizard.Api.Handler.PersistentCommand.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] (PersistentCommand U.UUID)
    :> "persistent-commands"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (PersistentCommand U.UUID))

list_POST
  :: Maybe String
  -> Maybe String
  -> PersistentCommand U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (PersistentCommand U.UUID))
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createPersistentCommand reqDto
