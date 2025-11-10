module Registry.Database.DAO.Organization.OrganizationDAO where

import Data.String
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Registry.Database.DAO.Common
import Registry.Database.Mapping.Organization.Organization ()
import Registry.Database.Mapping.Organization.OrganizationSimple ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import RegistryLib.Model.Organization.Organization
import RegistryLib.Model.Organization.OrganizationSimple
import Shared.Common.Util.Logger
import Shared.Common.Util.String

entityName = "organization"

findOrganizations :: AppContextM [Organization]
findOrganizations = createFindEntitiesFn entityName

findUsedOrganizations :: AppContextM [OrganizationSimple]
findUsedOrganizations = do
  let sql =
        "SELECT organization_id, name, logo \
        \FROM organization \
        \WHERE organization_id IN (SELECT DISTINCT nested.organization_id \
        \                          FROM (SELECT DISTINCT organization_id FROM knowledge_model_package \
        \                                UNION ALL \
        \                                SELECT DISTINCT organization_id FROM document_template \
        \                                UNION ALL \
        \                                SELECT DISTINCT organization_id FROM locale \
        \                         ) nested)"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findOrganizationByOrgId :: String -> AppContextM Organization
findOrganizationByOrgId organizationId = createFindEntityByFn entityName [("organization_id", organizationId)]

findOrganizationByOrgId' :: String -> AppContextM (Maybe Organization)
findOrganizationByOrgId' organizationId = createFindEntityByFn' entityName [("organization_id", organizationId)]

findOrganizationByToken :: String -> AppContextM Organization
findOrganizationByToken token = createFindEntityByFn entityName [("token", token)]

findOrganizationByToken' :: String -> AppContextM (Maybe Organization)
findOrganizationByToken' token = createFindEntityByFn' entityName [("token", token)]

findOrganizationByEmail :: String -> AppContextM Organization
findOrganizationByEmail email = createFindEntityByFn entityName [("email", email)]

findOrganizationByEmail' :: String -> AppContextM (Maybe Organization)
findOrganizationByEmail' email = createFindEntityByFn' entityName [("email", email)]

insertOrganization :: Organization -> AppContextM Int64
insertOrganization = createInsertFn entityName

updateOrganization :: Organization -> AppContextM Int64
updateOrganization org = do
  let sql =
        fromString
          "UPDATE organization SET organization_id = ?, name = ?, description = ?, email = ?, role = ?, token = ?, active = ?, logo = ?, created_at = ?, updated_at = ? WHERE organization_id = ?"
  let params = toRow org ++ [toField . T.pack $ org.organizationId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteOrganizations :: AppContextM Int64
deleteOrganizations = createDeleteEntitiesFn entityName

deleteOrganizationByOrgId :: String -> AppContextM Int64
deleteOrganizationByOrgId organizationId = createDeleteEntityByFn entityName [("organization_id", organizationId)]
