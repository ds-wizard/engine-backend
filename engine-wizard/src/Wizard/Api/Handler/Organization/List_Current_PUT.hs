module Wizard.Api.Handler.Organization.List_Current_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Api.Resource.Organization.OrganizationChangeJM ()
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Organization.OrganizationService

type List_Current_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] OrganizationChangeDTO
     :> "organizations"
     :> "current"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_current_PUT ::
     Maybe String -> OrganizationChangeDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_current_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "ORG_PERM"
      modifyOrganization reqDto
