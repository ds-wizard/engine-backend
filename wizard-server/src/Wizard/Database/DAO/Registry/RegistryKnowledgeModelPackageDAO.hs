module Wizard.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Registry.RegistryPackage ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Registry.RegistryPackage

entityName = "registry_knowledge_model_package"

findRegistryPackages :: AppContextM [RegistryPackage]
findRegistryPackages = createFindEntitiesFn entityName

insertRegistryPackage :: RegistryPackage -> AppContextM Int64
insertRegistryPackage = createInsertFn entityName

deleteRegistryPackages :: AppContextM Int64
deleteRegistryPackages = createDeleteEntitiesFn entityName
