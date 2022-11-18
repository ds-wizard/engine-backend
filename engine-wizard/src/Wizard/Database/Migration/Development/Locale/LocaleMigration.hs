module Wizard.Database.Migration.Development.Locale.LocaleMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Database.Migration.Development.Locale.Data.Locales
import Wizard.Model.Context.AppContext
import Wizard.Model.Locale.Locale
import Wizard.S3.Locale.LocaleS3
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(App/Locale) started"
  deleteLocales
  insertLocale localeCz
  logInfo _CMP_MIGRATION "(App/Locale) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  _ <- putLocale (f' "%s.json" [localeCz.code]) localeCzContent
  return ()
