module Wizard.Database.DAO.Organization.OrganizationDAO where

import Data.Bson

import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper)
import Wizard.Database.BSON.Organization.Organization ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Organization.Organization

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
