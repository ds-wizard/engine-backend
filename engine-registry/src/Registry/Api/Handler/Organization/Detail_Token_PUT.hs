module Registry.Api.Handler.Organization.Detail_Token_PUT where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Organization.OrganizationService

type Detail_Token_PUT
   = "organizations"
     :> Capture "orgId" String
     :> "token"
     :> QueryParam "hash" String
     :> Put '[ JSON] (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)

detail_token_PUT :: String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] OrganizationDTO)
detail_token_PUT orgId mHash = runInUnauthService $ addTraceUuidHeader =<< changeOrganizationTokenByHash orgId mHash
