module Wizard.Service.User.Profile.UserProfileValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Api.Resource.User.UserLocaleDTO

validateLocale :: UserLocaleDTO -> AppContextM ()
validateLocale reqDto = do
  case reqDto.uuid of
    Nothing -> return ()
    Just localeUuid -> do
      mLocale <- findLocaleByUuid' localeUuid
      case mLocale of
        Just locale ->
          if locale.enabled
            then return ()
            else throwError $ ValidationError [] (M.singleton "uuid" [_ERROR_VALIDATION__ABSENCE (U.toString localeUuid)])
        Nothing -> throwError $ ValidationError [] (M.singleton "uuid" [_ERROR_VALIDATION__ABSENCE (U.toString localeUuid)])
