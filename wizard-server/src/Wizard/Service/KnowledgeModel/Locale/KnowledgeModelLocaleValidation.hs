module Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Aeson (Value, eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

validateJsonContent :: BS.ByteString -> AppContextM ()
validateJsonContent jsonContent =
  case eitherDecodeStrict jsonContent :: Either String Value of
    Left reason -> throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_INVALID_JSON reason
    Right _ -> return ()

validateCodeUniqueness :: U.UUID -> String -> AppContextM ()
validateCodeUniqueness pkgUuid code = do
  mLocale <- findKnowledgeModelLocaleByPackageUuidAndCode' pkgUuid code
  when (isJust mLocale) (throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_CODE_UNIQUENESS code)
