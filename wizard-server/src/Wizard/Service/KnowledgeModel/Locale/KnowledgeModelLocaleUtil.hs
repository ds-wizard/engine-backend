module Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleUtil where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Char8 as BS

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Util.Gettext

extractLanguageCode :: BS.ByteString -> AppContextM String
extractLanguageCode poContent =
  case parsePoHeaderFields (BS.unpack poContent) of
    Left reason -> throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_INVALID_PO reason
    Right fields ->
      case getPoHeaderField "Language" fields of
        Just code -> return code
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_MISSING_LANGUAGE
