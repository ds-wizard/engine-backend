module Wizard.Api.Handler.DocumentTemplateDraft.File.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateFileChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateFileChangeDTO
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)
detail_PUT mTokenHeader mServerUrl reqDto tmlId fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyFile fileUuid reqDto
