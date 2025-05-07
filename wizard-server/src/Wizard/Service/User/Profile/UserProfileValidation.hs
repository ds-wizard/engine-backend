module Wizard.Service.User.Profile.UserProfileValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Locale.Model.Locale.Locale
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.User.UserLocaleDTO

validateLocale :: UserLocaleDTO -> AppContextM ()
validateLocale reqDto = do
  case reqDto.lId of
    Nothing -> return ()
    Just lId -> do
      mLocale <- findLocaleById' lId
      case mLocale of
        Just locale ->
          if locale.enabled
            then return ()
            else throwError $ ValidationError [] (M.singleton "id" [_ERROR_VALIDATION__ABSENCE lId])
        Nothing -> throwError $ ValidationError [] (M.singleton "id" [_ERROR_VALIDATION__ABSENCE lId])
