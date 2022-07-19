module Wizard.Api.Handler.Template.Asset.List_POST where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import qualified Data.Text as T
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Localization.Messages.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.Asset.TemplateAssetService

type List_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> MultipartForm Mem (MultipartData Mem)
     :> "templates"
     :> Capture "templateId" String
     :> "assets"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateAsset)

list_POST ::
     Maybe String
  -> Maybe String
  -> MultipartData Mem
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateAsset)
list_POST mTokenHeader mServerUrl multipartData tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
    addTraceUuidHeader =<< do
      let fs = files multipartData
      case L.find (\file -> fdInputName file == "file") fs of
        Just file -> do
          let content = fdPayload file
          createAsset tmlId (T.unpack . fdFileName $ file) (T.unpack . fdFileCType $ file) (BSL.toStrict content)
        Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
