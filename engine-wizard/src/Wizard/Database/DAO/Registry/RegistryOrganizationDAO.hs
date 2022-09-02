module Wizard.Database.DAO.Registry.RegistryOrganizationDAO where

import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Registry.RegistryOrganization ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Registry.RegistryOrganization

entityName = "registry_organization"

findRegistryOrganizations :: AppContextM [RegistryOrganization]
findRegistryOrganizations = createFindEntitiesFn entityName

insertRegistryOrganization :: RegistryOrganization -> AppContextM Int64
insertRegistryOrganization = createInsertFn entityName

deleteRegistryOrganizations :: AppContextM Int64
deleteRegistryOrganizations = createDeleteEntitiesFn entityName
