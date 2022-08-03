module Wizard.Api.Handler.PersistentCommand.Detail_Attempts_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService

type Detail_Attempts_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> "persistent-commands"
     :> Capture "pcUuid" String
     :> "attempts"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] PersistentCommandDetailDTO)

detail_attempts_POST ::
     Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] PersistentCommandDetailDTO)
detail_attempts_POST mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< runPersistentCommandById uuid
