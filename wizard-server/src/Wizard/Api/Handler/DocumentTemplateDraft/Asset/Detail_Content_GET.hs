module Wizard.Api.Handler.DocumentTemplateDraft.Asset.Detail_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Asset.DocumentTemplateAssetService

type Detail_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "assets"
    :> Capture "assetUuid" U.UUID
    :> "content"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_content_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_content_GET mTokenHeader mServerUrl tmlId assetUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      (asset, result) <- getAssetContent tmlId assetUuid
      let cdHeader = asset.contentType
      traceUuid <- asks traceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
