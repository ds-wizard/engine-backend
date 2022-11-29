module Wizard.Api.Handler.Locale.List_POST where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import qualified Data.Text as T
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Localization.Messages.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem (MultipartData Mem)
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> MultipartData Mem
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
list_POST mTokenHeader mServerUrl multipartData =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let fs = files multipartData
        let is = inputs multipartData
        case L.find (\file -> fdInputName file == "file") fs of
          Just file -> do
            let content = fdPayload file
            let fileName = T.unpack . fdFileName $ file
            name <- getStringFromInput "name" is
            description <- getStringFromInput "description" is
            code <- getStringFromInput "code" is
            localeId <- getStringFromInput "localeId" is
            version <- getStringFromInput "version" is
            license <- getStringFromInput "license" is
            readme <- getStringFromInput "readme" is
            recommendedAppVersion <- getStringFromInput "recommendedAppVersion" is
            let reqDto =
                  LocaleCreateDTO
                    { name = name
                    , description = description
                    , code = code
                    , localeId = localeId
                    , version = version
                    , license = license
                    , readme = readme
                    , recommendedAppVersion = recommendedAppVersion
                    }
            createLocale reqDto (BSL.toStrict content)
          Nothing -> throwError $ UserError _ERROR_VALIDATION__FILE_ABSENCE
