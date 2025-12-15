module Registry.Api.Handler.PersistentCommand.List_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Service.PersistentCommand.PersistentCommandService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

list_POST
  :: Maybe String
  -> PersistentCommand String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (PersistentCommand String))
list_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createPersistentCommand reqDto
