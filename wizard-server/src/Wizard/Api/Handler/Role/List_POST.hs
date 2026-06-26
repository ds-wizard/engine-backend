module Wizard.Api.Handler.Role.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Role.RoleService
import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Api.Resource.User.RoleChangeJM ()
import WizardLib.Public.Api.Resource.User.RoleListJM ()
import WizardLib.Public.Model.User.RoleList

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] RoleChangeDTO
    :> "roles"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] RoleList)

list_POST
  :: Maybe String
  -> Maybe String
  -> RoleChangeDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] RoleList)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createRole reqDto
