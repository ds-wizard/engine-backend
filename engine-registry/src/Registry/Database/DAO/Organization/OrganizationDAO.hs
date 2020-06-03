module Registry.Database.DAO.Organization.OrganizationDAO where

import Control.Lens ((^.))
import Data.Bson

import LensesConfig
import Registry.Database.BSON.Organization.Organization ()
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextLenses ()
import Registry.Model.Organization.Organization
import Shared.Database.DAO.Common

entityName = "organization"

collection = "organizations"

findOrganizations :: AppContextM [Organization]
findOrganizations = createFindEntitiesFn collection

findOrganizationByOrgId :: String -> AppContextM Organization
findOrganizationByOrgId = createFindEntityByFn collection entityName "organizationId"

findOrganizationByOrgId' :: String -> AppContextM (Maybe Organization)
findOrganizationByOrgId' = createFindEntityByFn' collection entityName "organizationId"

findOrganizationByToken :: String -> AppContextM Organization
findOrganizationByToken = createFindEntityByFn collection entityName "token"

findOrganizationByToken' :: String -> AppContextM (Maybe Organization)
findOrganizationByToken' = createFindEntityByFn' collection entityName "token"

findOrganizationByEmail :: String -> AppContextM Organization
findOrganizationByEmail = createFindEntityByFn collection entityName "email"

findOrganizationByEmail' :: String -> AppContextM (Maybe Organization)
findOrganizationByEmail' = createFindEntityByFn' collection entityName "email"

insertOrganization :: Organization -> AppContextM Value
insertOrganization = createInsertFn collection

updateOrganization :: Organization -> AppContextM ()
updateOrganization org = createUpdateByFn collection "organizationId" (org ^. organizationId) org

deleteOrganizations :: AppContextM ()
deleteOrganizations = createDeleteEntitiesFn collection

deleteOrganizationByOrgId :: String -> AppContextM ()
deleteOrganizationByOrgId = createDeleteEntityByFn collection "organizationId"
