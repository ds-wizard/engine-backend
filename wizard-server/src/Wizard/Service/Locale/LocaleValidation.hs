module Wizard.Service.Locale.LocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Service.Coordinate.CoordinateValidation
import Shared.Coordinate.Util.Coordinate
import Shared.Locale.Constant.Locale
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Model.Context.AppContext
import WizardLib.Public.Localization.Messages.Public

validateLocaleCreate :: LocaleCreateDTO -> String -> AppContextM ()
validateLocaleCreate reqDto organizationId = do
  validateCoordinatePartFormat "localeId" reqDto.localeId
  validateLocaleIdUniqueness (buildCoordinate organizationId reqDto.localeId reqDto.version)

validateLocaleChange :: LocaleChangeDTO -> Locale -> AppContextM ()
validateLocaleChange reqDto locale = do
  when (not locale.enabled && not reqDto.enabled && reqDto.defaultLocale) (throwError . UserError $ _ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT)
  when (locale.defaultLocale && reqDto.defaultLocale && not reqDto.enabled) (throwError . UserError $ _ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE)

validateLocaleDeletion :: Locale -> AppContextM ()
validateLocaleDeletion locale = do
  when locale.defaultLocale (throwError . UserError $ _ERROR_VALIDATION__DEFAULT_LOCALE_DELETION)
  when (locale.lId == defaultLocaleId) (throwError . UserError $ _ERROR_VALIDATION__DEFAULT_WIZARD_LOCALE_DELETION)

validateLocaleIdUniqueness :: String -> AppContextM ()
validateLocaleIdUniqueness lclId = do
  mLocale <- findLocaleById' lclId
  case mLocale of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__LCL_ID_UNIQUENESS lclId
