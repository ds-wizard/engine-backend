module Wizard.Api.Handler.Template.Asset.Detail_Content_GET where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Servant hiding (contentType)

import LensesConfig
import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type Detail_Content_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> "assets"
     :> Capture "assetUuid" String
     :> "content"
     :> Get '[ OctetStream] (Headers '[ Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_content_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_content_GET mTokenHeader mServerUrl tmlId assetUuid =
  getServiceTokenOrAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ do
      (asset, result) <- getTemplateAssetContent tmlId assetUuid
      let cdHeader = asset ^. contentType
      traceUuid <- asks _appContextTraceUuid
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream $ result
