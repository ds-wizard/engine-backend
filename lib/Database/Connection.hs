module Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.MongoDB (allCollections)
import Database.Persist.MongoDB
import Network

import LensesConfig

createDatabaseConnectionPool dswConfig = do
  dbPool <- createMongoDBPool dbName dbHost dbPort dbCred 1 1 1
  verifyDatabaseConnectionPool dbPool
  return dbPool
  where
    appConfigDatabase = dswConfig ^. databaseConfig
    dbHost = appConfigDatabase ^. host
    dbPort = PortNumber (fromInteger (appConfigDatabase ^. port) :: PortNumber) :: PortID
    dbName = pack (appConfigDatabase ^. databaseName)
    dbCred =
      if appConfigDatabase ^. authEnabled
        then Just $ MongoAuth (pack $ appConfigDatabase ^. username) (pack $ appConfigDatabase ^. password)
        else Nothing

verifyDatabaseConnectionPool dbPool = do
  _ <- runMongoDBPoolDef allCollections dbPool
  return ()
