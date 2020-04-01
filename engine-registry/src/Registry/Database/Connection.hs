module Registry.Database.Connection where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
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
    serverConfigDatabase = serverConfig ^. database
    dbHost = serverConfigDatabase ^. host
    dbPort = PortNumber (fromInteger (serverConfigDatabase ^. port) :: PortNumber) :: PortID
    dbName = pack (serverConfigDatabase ^. databaseName)
    dbCred =
      if serverConfigDatabase ^. authEnabled
        then Just $
             MongoAuth
               (pack . fromMaybe "" $ serverConfigDatabase ^. username)
               (pack . fromMaybe "" $ serverConfigDatabase ^. password)
        else Nothing

verifyDatabaseConnectionPool dbPool = do
  _ <- runMongoDBPoolDef allCollections dbPool
  return ()
