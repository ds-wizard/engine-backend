module Wizard.Api.Handler.PersistentCommand.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> QueryParam "state" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page PersistentCommandDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page PersistentCommandDTO))
list_GET mTokenHeader mServerUrl mStatesL mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let states =
              case mStatesL of
                Just statesL -> splitOn "," statesL
                Nothing -> []
        getPersistentCommandsPage states (Pageable mPage mSize) (parseSortQuery mSort)
