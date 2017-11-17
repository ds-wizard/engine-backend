module Database.DAO.Organization.OrganizationDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       (find, findOne, select, insert, fetch, save, merge, delete,
        deleteOne, (=:), rest)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Error
import Common.Types
import Common.Context
import Database.BSON.Organization.Organization
import Database.DAO.Common
import Model.Organization.Organization

orgCollection = "organizations"

findOrganization :: Context -> IO (Either AppError Organization)
findOrganization context = do
  let action = findOne $ select [] orgCollection
  maybeOrganization <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeOrganization

insertOrganization :: Context -> Organization -> IO Value
insertOrganization context organization = do
  let action = insert orgCollection (toBSON organization)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateOrganization :: Context -> Organization -> IO ()
updateOrganization context organization = do
  let action =
        fetch (select [] orgCollection) >>=
        save orgCollection . merge (toBSON organization)
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteOrganizations :: Context -> IO ()
deleteOrganizations context = do
  let action = delete $ select [] orgCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
