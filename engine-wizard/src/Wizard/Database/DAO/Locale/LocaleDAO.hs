module Wizard.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Locale.Locale ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Locale.Locale

entityName = "locale"

findLocales :: AppContextM [Locale]
findLocales = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findLocaleById :: String -> AppContextM Locale
findLocaleById uuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertLocale :: Locale -> AppContextM Int64
insertLocale = createInsertFn entityName

deleteLocales :: AppContextM Int64
deleteLocales = createDeleteEntitiesFn entityName
