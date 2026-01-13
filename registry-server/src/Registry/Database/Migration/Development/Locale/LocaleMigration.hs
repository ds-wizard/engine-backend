module Registry.Database.Migration.Development.Locale.LocaleMigration where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Locale.LocaleS3
import Shared.Common.Util.Logger
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Locale/Locale) started"
  deleteLocales
  insertLocale localeNl
  logInfo _CMP_MIGRATION "(Locale/Locale) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  _ <- putLocale localeNl.uuid "wizard.json" localeNlContent
  _ <- putLocale localeNl.uuid "mail.po" localeNlContent
  return ()
