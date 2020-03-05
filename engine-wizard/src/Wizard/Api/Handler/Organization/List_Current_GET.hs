module Wizard.Api.Handler.Organization.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Organization.OrganizationService

type List_Current_GET
   = Header "Authorization" String
     :> "organizations"
     :> "current"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_current_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_current_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService -> runInAuthService $ addTraceUuidHeader =<< getOrganization
