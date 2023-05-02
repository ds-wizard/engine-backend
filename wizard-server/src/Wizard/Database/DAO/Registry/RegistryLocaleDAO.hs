module Wizard.Database.DAO.Registry.RegistryLocaleDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Registry.RegistryLocale ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Registry.RegistryLocale

entityName = "registry_locale"

findRegistryLocales :: AppContextM [RegistryLocale]
findRegistryLocales = createFindEntitiesFn entityName

insertRegistryLocale :: RegistryLocale -> AppContextM Int64
insertRegistryLocale = createInsertFn entityName

deleteRegistryLocales :: AppContextM Int64
deleteRegistryLocales = createDeleteEntitiesFn entityName
