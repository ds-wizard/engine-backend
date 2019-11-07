module Database.DAO.Organization.OrganizationDAO where

import Control.Lens ((^.))
import Data.Bson

import Database.BSON.Organization.Organization ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Organization.Organization
import Util.Helper (createHeeHelper, createHemHelper)

entityName = "organization"

collection = "organizations"

findOrganizations :: AppContextM (Either AppError [Organization])
findOrganizations = createFindEntitiesFn collection

findOrganizationByOrgId :: String -> AppContextM (Either AppError Organization)
findOrganizationByOrgId = createFindEntityByFn collection entityName "organizationId"

findOrganizationByToken :: String -> AppContextM (Either AppError Organization)
findOrganizationByToken = createFindEntityByFn collection entityName "token"

findOrganizationByEmail :: String -> AppContextM (Either AppError Organization)
findOrganizationByEmail = createFindEntityByFn collection entityName "email"

insertOrganization :: Organization -> AppContextM Value
insertOrganization = createInsertFn collection

updateOrganization :: Organization -> AppContextM ()
updateOrganization org = createUpdateByFn collection "organizationId" (org ^. organizationId) org

deleteOrganizations :: AppContextM ()
deleteOrganizations = createDeleteEntitiesFn collection

deleteOrganizationByOrgId :: String -> AppContextM ()
deleteOrganizationByOrgId = createDeleteEntityByFn collection "organizationId"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindOrganizations callback = createHeeHelper findOrganizations callback

-- --------------------------------
heFindOrganizationByOrgId orgId callback = createHeeHelper (findOrganizationByOrgId orgId) callback

hmFindOrganizationByOrgId orgId callback = createHemHelper (findOrganizationByOrgId orgId) callback

-- --------------------------------
heFindOrganizationByToken token callback = createHeeHelper (findOrganizationByToken token) callback

-- --------------------------------
hmFindOrganizationByEmail email callback = createHemHelper (findOrganizationByEmail email) callback
