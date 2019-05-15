module Database.DAO.Organization.OrganizationDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       (delete, fetch, findOne, insert, merge, save, select)

import Database.BSON.Organization.Organization ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.Organization.Organization

entityName = "organization"

orgCollection = "organizations"

findOrganization :: AppContextM (Either AppError Organization)
findOrganization = do
  let action = findOne $ select [] orgCollection
  maybeOrganization <- runDB action
  return . deserializeMaybeEntity entityName "nothing" $ maybeOrganization

insertOrganization :: Organization -> AppContextM Value
insertOrganization organization = do
  let action = insert orgCollection (toBSON organization)
  runDB action

updateOrganization :: Organization -> AppContextM ()
updateOrganization organization = do
  let action = fetch (select [] orgCollection) >>= save orgCollection . merge (toBSON organization)
  runDB action

deleteOrganizations :: AppContextM ()
deleteOrganizations = do
  let action = delete $ select [] orgCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindOrganization callback = do
  eitherOrganization <- findOrganization
  case eitherOrganization of
    Right organization -> callback organization
    Left error -> return . Left $ error
