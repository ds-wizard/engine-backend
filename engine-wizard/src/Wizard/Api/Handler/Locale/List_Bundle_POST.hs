module Wizard.Api.Handler.Locale.List_Bundle_POST where

import qualified Data.List as L
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Localization.Messages.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.LocaleBundle.LocaleBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem (MultipartData Mem)
    :> "locales"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

list_bundle_POST
  :: Maybe String
  -> Maybe String
  -> MultipartData Mem
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
list_bundle_POST mTokenHeader mServerUrl multipartData =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let fs = files multipartData
        case L.find (\file -> fdInputName file == "file") fs of
          Just file -> do
            let content = fdPayload file
            importAndConvertLocaleBundle content False
          Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
