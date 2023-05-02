module Wizard.Api.Handler.PersistentCommand.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Service.PersistentCommand.PersistentCommandService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PersistentCommand
    :> "persistent-commands"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PersistentCommand)

list_POST
  :: Maybe String
  -> Maybe String
  -> PersistentCommand
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PersistentCommand)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createPersistentCommand reqDto
