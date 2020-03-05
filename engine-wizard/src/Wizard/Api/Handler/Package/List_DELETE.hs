module Wizard.Api.Handler.Package.List_DELETE where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type List_DELETE
   = Header "Authorization" String
     :> "packages"
     :> QueryParam "organizationId" String
     :> QueryParam "kmId" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_DELETE ::
     Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mOrganizationId mKmId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "PM_WRITE_PERM"
      let queryParams = catMaybes [(,) "organizationId" <$> mOrganizationId, (,) "kmId" <$> mKmId]
      deletePackagesByQueryParams queryParams
      return NoContent
