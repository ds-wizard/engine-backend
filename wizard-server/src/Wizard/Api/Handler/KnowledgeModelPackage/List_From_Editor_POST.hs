module Wizard.Api.Handler.KnowledgeModelPackage.List_From_Editor_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishService

type List_From_Editor_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] PackagePublishEditorDTO
    :> "knowledge-model-packages"
    :> "from-editor"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_from_editor_POST
  :: Maybe String
  -> Maybe String
  -> PackagePublishEditorDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_from_editor_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< publishPackageFromKnowledgeModelEditor reqDto
