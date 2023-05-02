module Wizard.Api.Handler.Package.List_DELETE where

import Data.Maybe (catMaybes)
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "packages"
    :> QueryParam "organizationId" String
    :> QueryParam "kmId" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl mOrganizationId mKmId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "km_id" <$> mKmId]
        deletePackagesByQueryParams queryParams
        return NoContent
