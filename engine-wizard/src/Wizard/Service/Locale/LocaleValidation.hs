module Wizard.Service.Locale.LocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Constant.Locale
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Util.Coordinate
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateLocaleCreate :: LocaleCreateDTO -> String -> AppContextM ()
validateLocaleCreate reqDto organizationId = validateLocaleIdUniqueness (buildCoordinate organizationId reqDto.localeId reqDto.version)

validateLocaleChange :: LocaleChangeDTO -> Locale -> AppContextM ()
validateLocaleChange reqDto locale = do
  when (not locale.enabled && not reqDto.enabled && reqDto.defaultLocale) (throwError . UserError $ _ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT)
  when (locale.defaultLocale && reqDto.defaultLocale && not reqDto.enabled) (throwError . UserError $ _ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE)

validateLocaleDeletation :: Locale -> AppContextM ()
validateLocaleDeletation locale = do
  when locale.defaultLocale (throwError . UserError $ _ERROR_VALIDATION__DEFAULT_LOCALE_DELETATION)
  when (locale.lId == defaultLocaleId) (throwError . UserError $ _ERROR_VALIDATION__DEFAULT_WIZARD_LOCALE_DELETATION)

validateLocaleIdUniqueness :: String -> AppContextM ()
validateLocaleIdUniqueness lclId = do
  mLocale <- findLocaleById' lclId
  case mLocale of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__LCL_ID_UNIQUENESS lclId
