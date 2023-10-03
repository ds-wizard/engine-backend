module Wizard.Database.DAO.Registry.RegistryOrganizationDAO where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.String
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

deleteRegistryOrganizationsByOrganizationIds :: [String] -> AppContextM ()
deleteRegistryOrganizationsByOrganizationIds organizationIds = do
  let sql =
        fromString $
          f' "DELETE FROM %s WHERE organization_id IN (%s)" [entityName, generateQuestionMarks organizationIds]
  let params = organizationIds
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()
