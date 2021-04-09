module Shared.Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.MongoDB hiding (host)
import Database.Persist.MongoDB
import Network.Socket

import LensesConfig

createDatabaseConnectionPool serverConfig = do
  dbPool <- createMongoDBPool dbName dbHost dbPort dbCred dbConnectionPoolSize dbStripeSize dbConnectionIdleTime
  verifyDatabaseConnectionPool dbPool
  return dbPool
  where
    dbHost = serverConfig ^. host
    dbPort = PortNumber (fromInteger (serverConfig ^. port) :: PortNumber) :: PortID
    dbName = pack (serverConfig ^. databaseName)
    dbConnectionPoolSize = serverConfig ^. connectionPoolSize
    dbStripeSize = serverConfig ^. stripeSize
    dbConnectionIdleTime = realToFrac $ serverConfig ^. connectionIdleTime
    dbCred =
      if serverConfig ^. authEnabled
        then Just $ MongoAuth (pack $ serverConfig ^. username) (pack $ serverConfig ^. password)
        else Nothing

verifyDatabaseConnectionPool dbPool = do
  _ <- runMongoDBPoolDef allCollections dbPool
  return ()
