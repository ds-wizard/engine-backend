module Registry.Api.Handler.PersistentCommand.List_POST where

import qualified Data.UUID as U
import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.PersistentCommand.PersistentCommandService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

list_POST
  :: Maybe String
  -> PersistentCommand U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (PersistentCommand U.UUID))
list_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createPersistentCommand reqDto
