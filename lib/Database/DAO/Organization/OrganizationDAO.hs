module Database.DAO.Organization.OrganizationDAO where

import Data.Bson

import Database.BSON.Organization.Organization ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Organization.Organization
import Util.Helper (createHeeHelper)

entityName = "organization"

collection = "organizations"

findOrganization :: AppContextM (Either AppError Organization)
findOrganization = createFindEntityFn collection entityName

insertOrganization :: Organization -> AppContextM Value
insertOrganization = createInsertFn collection

updateOrganization :: Organization -> AppContextM ()
updateOrganization = createUpdateFn collection

deleteOrganizations :: AppContextM ()
deleteOrganizations = createDeleteEntitiesFn collection

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindOrganization callback = createHeeHelper findOrganization callback
