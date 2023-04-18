module Wizard.Api.Handler.App.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppCreateJM ()
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] AppCreateDTO
    :> "apps"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] AppDTO)

list_POST
  :: Maybe String -> Maybe String -> AppCreateDTO -> BaseContextM (Headers '[Header "x-trace-uuid" String] AppDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        ia <- isAdmin
        if ia
          then createAppByAdmin reqDto
          else registerApp reqDto
