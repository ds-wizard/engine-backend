module Registry.Database.DAO.Organization.OrganizationDAO where

import Control.Lens ((^.))
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Registry.Database.DAO.Common
import Registry.Database.Mapping.Organization.Organization ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Model.Organization.Organization

entityName = "organization"

findOrganizations :: AppContextM [Organization]
findOrganizations = createFindEntitiesFn entityName

findOrganizationByOrgId :: String -> AppContextM Organization
findOrganizationByOrgId = createFindEntityByFn entityName "organization_id"

findOrganizationByOrgId' :: String -> AppContextM (Maybe Organization)
findOrganizationByOrgId' = createFindEntityByFn' entityName "organization_id"

findOrganizationByToken :: String -> AppContextM Organization
findOrganizationByToken = createFindEntityByFn entityName "token"

findOrganizationByToken' :: String -> AppContextM (Maybe Organization)
findOrganizationByToken' = createFindEntityByFn' entityName "token"

findOrganizationByEmail :: String -> AppContextM Organization
findOrganizationByEmail = createFindEntityByFn entityName "email"

findOrganizationByEmail' :: String -> AppContextM (Maybe Organization)
findOrganizationByEmail' = createFindEntityByFn' entityName "email"

insertOrganization :: Organization -> AppContextM Int64
insertOrganization = createInsertFn entityName

updateOrganization :: Organization -> AppContextM Int64
updateOrganization org = do
  let params = toRow org ++ [toField . T.pack $ org ^. organizationId]
  let action conn =
        execute
          conn
          "UPDATE organization SET organization_id = ?, name = ?, description = ?, email = ?, role = ?, token = ?, active = ?, logo = ?, created_at = ?, updated_at = ? WHERE organization_id = ?"
          params
  runDB action

deleteOrganizations :: AppContextM Int64
deleteOrganizations = createDeleteEntitiesFn entityName

deleteOrganizationByOrgId :: String -> AppContextM Int64
deleteOrganizationByOrgId = createDeleteEntityByFn entityName "organization_id"
