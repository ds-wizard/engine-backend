module Wizard.Api.Handler.Config.List_App_Logo_POST where

import qualified Data.List as L
import qualified Data.Text as T
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Localization.Messages.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigLogoService

type List_App_Logo_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem (MultipartData Mem)
    :> "configs"
    :> "app"
    :> "logo"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_app_logo_POST
  :: Maybe String
  -> Maybe String
  -> MultipartData Mem
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_app_logo_POST mTokenHeader mServerUrl multipartData =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let fs = files multipartData
        case L.find (\file -> fdInputName file == "file") fs of
          Just file -> do
            let content = fdPayload file
            let contentType = fdFileCType file
            uploadLogo (T.unpack . fdFileName $ file) (T.unpack contentType) content
            return NoContent
          Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
