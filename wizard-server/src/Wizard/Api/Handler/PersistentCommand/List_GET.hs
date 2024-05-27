module Wizard.Api.Handler.PersistentCommand.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.PersistentCommand.PersistentCommandService
import WizardLib.Public.Api.Resource.PersistentCommand.PersistentCommandListJM ()
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "persistent-commands"
    :> QueryParam "state" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page PersistentCommandList))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page PersistentCommandList))
list_GET mTokenHeader mServerUrl mStatesL mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let states =
              case mStatesL of
                Just statesL -> splitOn "," statesL
                Nothing -> []
        getPersistentCommandsPage states (Pageable mPage mSize) (parseSortQuery mSort)
