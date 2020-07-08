module Shared.Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.MongoDB hiding (host)
import Database.Persist.MongoDB
import Network.Socket

import LensesConfig

createDatabaseConnectionPool serverConfig = do
  dbPool <- createMongoDBPool dbName dbHost dbPort dbCred 1 1 1
  verifyDatabaseConnectionPool dbPool
  return dbPool
  where
    dbHost = serverConfig ^. host
    dbPort = PortNumber (fromInteger (serverConfig ^. port) :: PortNumber) :: PortID
    dbName = pack (serverConfig ^. databaseName)
    dbCred =
      if serverConfig ^. authEnabled
        then Just $ MongoAuth (pack $ serverConfig ^. username) (pack $ serverConfig ^. password)
        else Nothing

verifyDatabaseConnectionPool dbPool = do
  _ <- runMongoDBPoolDef allCollections dbPool
  return ()
