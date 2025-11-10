module Wizard.Api.Handler.KnowledgeModelPackage.List_From_Migration_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishService

type List_From_Migration_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PackagePublishMigrationDTO
    :> "knowledge-model-packages"
    :> "from-migration"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_from_migration_POST
  :: Maybe String
  -> Maybe String
  -> PackagePublishMigrationDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_from_migration_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< publishPackageFromMigration reqDto
