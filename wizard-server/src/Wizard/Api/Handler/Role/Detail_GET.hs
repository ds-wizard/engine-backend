module Wizard.Api.Handler.Role.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Role.RoleService
import WizardLib.Public.Api.Resource.User.RoleListJM ()
import WizardLib.Public.Model.User.RoleList

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "roles"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] RoleList)

detail_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] RoleList)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getRole uuid
