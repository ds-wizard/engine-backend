module Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService

type Detail_Locales_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Capture "localeUuid" U.UUID
    :> "content"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_locales_content_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_locales_content_GET mTokenHeader mServerUrl pkgUuid localeUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      content <- getLocaleContent pkgUuid localeUuid
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader "application/octet-stream" . FileStream $ content
