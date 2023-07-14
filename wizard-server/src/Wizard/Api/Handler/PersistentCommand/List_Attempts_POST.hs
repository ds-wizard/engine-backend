module Wizard.Api.Handler.PersistentCommand.List_Attempts_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService

type List_Attempts_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> "attempts"
    :> Verb 'POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_attempts_POST :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_attempts_POST mTokenHeader mServerUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        runPersistentCommands'
        return NoContent
