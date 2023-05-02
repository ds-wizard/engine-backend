module Registry.Api.Handler.Organization.Detail_Token_PUT where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Organization.OrganizationService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_Token_PUT =
  "organizations"
    :> Capture "orgId" String
    :> "token"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OrganizationDTO)

detail_token_PUT :: String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] OrganizationDTO)
detail_token_PUT orgId hash =
  runInUnauthService Transactional $ addTraceUuidHeader =<< changeOrganizationTokenByHash orgId hash
