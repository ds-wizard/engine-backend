module Wizard.Api.Handler.Package.List_From_Migration_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.Publish.PackagePublishService

type List_From_Migration_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PackagePublishMigrationDTO
    :> "packages"
    :> "from-migration"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageSimpleDTO)

list_from_migration_POST
  :: Maybe String
  -> Maybe String
  -> PackagePublishMigrationDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageSimpleDTO)
list_from_migration_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< publishPackageFromMigration reqDto
