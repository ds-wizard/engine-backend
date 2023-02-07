module Wizard.Api.Handler.DocumentTemplateDraft.File.Detail_Content_PUT where

import qualified Data.Text as T
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService

type Detail_Content_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[PlainText] T.Text
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> "content"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)

detail_content_PUT
  :: Maybe String
  -> Maybe String
  -> T.Text
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)
detail_content_PUT mTokenHeader mServerUrl reqContent tmlId fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyFileContent fileUuid (T.unpack reqContent)
