module Registry.Api.Handler.Organization.List_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

list_POST :: Maybe String -> OrganizationCreateDTO -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
list_POST mTokenHeader reqDto mCallbackUrl =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $
      addTraceUuidHeader =<< createOrganization reqDto mCallbackUrl
