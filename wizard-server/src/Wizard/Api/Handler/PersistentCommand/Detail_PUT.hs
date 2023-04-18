module Wizard.Api.Handler.PersistentCommand.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeJM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PersistentCommandChangeDTO
    :> "persistent-commands"
    :> Capture "pcUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> PersistentCommandChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PersistentCommandDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPersistentCommand uuid reqDto
