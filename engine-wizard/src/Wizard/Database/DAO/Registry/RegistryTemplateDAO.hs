module Wizard.Database.DAO.Registry.RegistryTemplateDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Registry.RegistryTemplate ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Registry.RegistryTemplate

entityName = "registry_template"

findRegistryTemplates :: AppContextM [RegistryTemplate]
findRegistryTemplates = createFindEntitiesFn entityName

insertRegistryTemplate :: RegistryTemplate -> AppContextM Int64
insertRegistryTemplate = createInsertFn entityName

deleteRegistryTemplates :: AppContextM Int64
deleteRegistryTemplates = createDeleteEntitiesFn entityName
