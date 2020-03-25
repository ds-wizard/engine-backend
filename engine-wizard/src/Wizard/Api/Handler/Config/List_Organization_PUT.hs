module Wizard.Api.Handler.Config.List_Organization_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Api.Resource.Organization.OrganizationChangeJM ()
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Organization.OrganizationService

type List_Organization_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] OrganizationChangeDTO
     :> "configs"
     :> "organization"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_organization_PUT ::
     Maybe String -> OrganizationChangeDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_organization_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyOrganization reqDto
