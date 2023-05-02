module Wizard.Api.Handler.DocumentTemplateDraft.File.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)
detail_GET mTokenHeader mServerUrl tmlId fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getFile fileUuid
