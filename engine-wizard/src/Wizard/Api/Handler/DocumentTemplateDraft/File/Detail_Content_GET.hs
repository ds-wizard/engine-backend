module Wizard.Api.Handler.DocumentTemplateDraft.File.Detail_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService

type Detail_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> "content"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_content_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_content_GET mTokenHeader mServerUrl tmlId fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      file <- getFile fileUuid
      let cdHeader = "text/plain;charset=utf-8"
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream . TE.encodeUtf8 . T.pack $ file.content
