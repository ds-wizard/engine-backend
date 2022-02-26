module Wizard.Api.Handler.App.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppCreateJM ()
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.App.AppService

type List_POST
   = Header "Host" String
     :> ReqBody '[ SafeJSON] AppCreateDTO
     :> "apps"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppDTO)

list_POST :: Maybe String -> AppCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppDTO)
list_POST mServerUrl reqDto = runInUnauthService mServerUrl $ addTraceUuidHeader =<< createApp reqDto
