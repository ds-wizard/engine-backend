module Wizard.Api.Handler.Config.List_App_Logo_POST where

import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Common.FileDTO
import Shared.Common.Api.Resource.Common.FileJM ()
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.App.AppConfigLogoService

type List_App_Logo_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "configs"
    :> "app"
    :> "logo"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_app_logo_POST
  :: Maybe String
  -> Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_app_logo_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        uploadLogo reqDto.fileName reqDto.contentType reqDto.content
        return NoContent
