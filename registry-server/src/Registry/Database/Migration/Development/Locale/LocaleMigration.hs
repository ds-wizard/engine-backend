module Registry.Database.Migration.Development.Locale.LocaleMigration where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Locale.LocaleS3
import Registry.Util.Logger
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(App/Locale) started"
  deleteLocales
  insertLocale localeNl
  logInfo _CMP_MIGRATION "(App/Locale) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  _ <- putLocale localeNl.lId localeNlContent
  return ()
