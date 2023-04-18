module Wizard.Api.Handler.DocumentTemplateDraft.File.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "files"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateFileList])

list_GET
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [DocumentTemplateFileList])
list_GET mTokenHeader mServerUrl tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getFiles tmlId
