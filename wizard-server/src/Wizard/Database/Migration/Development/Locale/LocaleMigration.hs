module Wizard.Database.Migration.Development.Locale.LocaleMigration where

import Data.Foldable (traverse_)

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.Locale
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.Locale.LocaleS3

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Limit/Locale) started"
  deleteLocales
  insertLocale localeDefaultEn
  insertLocale localeNl
  insertLocale localeDe
  insertLocale differentLocale
  logInfo _CMP_MIGRATION "(Limit/Locale) ended"

runS3Migration :: AppContextM ()
runS3Migration =
  traverse_
    ( \(lclId, content) -> do
        _ <- putLocale lclId "wizard.json" content
        _ <- putLocale lclId "mail.po" content
        return ()
    )
    [ (localeNl.lId, localeNlContent)
    , (localeDe.lId, localeDeContent)
    , (differentLocale.lId, differentLocaleContent)
    ]
