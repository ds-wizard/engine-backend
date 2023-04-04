module Wizard.Api.Handler.PersistentCommand.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> Capture "pcUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)
detail_GET mTokenHeader mServerUrl pcUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getPersistentCommandById pcUuid
