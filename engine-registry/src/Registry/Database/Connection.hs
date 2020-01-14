module Registry.Database.Connection where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Text
import Database.MongoDB hiding (host)
import Database.Persist.MongoDB
import Network.Socket

import LensesConfig

createDatabaseConnectionPool appConfig = do
  dbPool <- createMongoDBPool dbName dbHost dbPort dbCred 1 1 1
  verifyDatabaseConnectionPool dbPool
  return dbPool
  where
    appConfigDatabase = appConfig ^. database
    dbHost = appConfigDatabase ^. host
    dbPort = PortNumber (fromInteger (appConfigDatabase ^. port) :: PortNumber) :: PortID
    dbName = pack (appConfigDatabase ^. databaseName)
    dbCred =
      if appConfigDatabase ^. authEnabled
        then Just $
             MongoAuth
               (pack . fromMaybe "" $ appConfigDatabase ^. username)
               (pack . fromMaybe "" $ appConfigDatabase ^. password)
        else Nothing

verifyDatabaseConnectionPool dbPool = do
  _ <- runMongoDBPoolDef allCollections dbPool
  return ()
