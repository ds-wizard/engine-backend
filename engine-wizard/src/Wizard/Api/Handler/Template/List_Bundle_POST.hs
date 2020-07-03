module Wizard.Api.Handler.Template.List_Bundle_POST where

import qualified Data.List as L
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.TemplateBundle.TemplateBundleService

type List_Bundle_POST
   = Header "Authorization" String
     :> MultipartForm Mem (MultipartData Mem)
     :> "templates"
     :> "bundle"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Template)

list_bundle_POST ::
     Maybe String -> MultipartData Mem -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Template)
list_bundle_POST mTokenHeader multipartData =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let fs = files multipartData
      case L.find (\file -> fdInputName file == "file") fs of
        Just file -> do
          let content = fdPayload file
          importAndConvertTemplateBundle content
        Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
