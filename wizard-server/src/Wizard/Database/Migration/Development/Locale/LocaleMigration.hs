module Wizard.Database.Migration.Development.Locale.LocaleMigration where

import Shared.Common.Constant.Component
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Model.Context.AppContext
import Wizard.S3.Locale.LocaleS3
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(App/Locale) started"
  deleteLocales
  insertLocale localeDefaultEn
  insertLocale localeNl
  insertLocale localeDe
  insertLocale differentLocale
  logInfo _CMP_MIGRATION "(App/Locale) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  _ <- putLocale localeNl.lId localeNlContent
  _ <- putLocale localeDe.lId localeDeContent
  _ <- putLocale differentLocale.lId differentLocaleContent
  return ()
