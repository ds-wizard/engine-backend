module Wizard.Database.DAO.Organization.OrganizationDAO where

import Data.Bson

import Wizard.Database.BSON.Organization.Organization ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Organization.Organization

entityName = "organization"

collection = "organizations"

findOrganization :: AppContextM Organization
findOrganization = createFindEntityFn collection entityName

insertOrganization :: Organization -> AppContextM Value
insertOrganization = createInsertFn collection

updateOrganization :: Organization -> AppContextM ()
updateOrganization = createUpdateFn collection

deleteOrganizations :: AppContextM ()
deleteOrganizations = createDeleteEntitiesFn collection
