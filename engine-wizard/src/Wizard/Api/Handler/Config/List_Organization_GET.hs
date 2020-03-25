module Wizard.Api.Handler.Config.List_Organization_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Organization.OrganizationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Organization.OrganizationService

type List_Organization_GET
   = Header "Authorization" String
     :> "configs"
     :> "organization"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

list_organization_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
list_organization_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      getOrganization
