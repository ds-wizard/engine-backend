module Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.Persist.MongoDB
import Network

import Database.Migration.Migration
import LensesConfig
import Model.Config.DSWConfig

createDBConn dswConfig afterSuccess =
  let appConfigDatabase = dswConfig ^. databaseConfig
      dbHost = appConfigDatabase ^. host
      dbPort = PortNumber (fromInteger (appConfigDatabase ^. port) :: PortNumber) :: PortID
      dbName = pack (appConfigDatabase ^. databaseName)
      dbCred = Just $ MongoAuth (pack $ appConfigDatabase ^. username) (pack $ appConfigDatabase ^. password)
  in if appConfigDatabase ^. authEnabled
       then withMongoDBConn dbName dbHost dbPort dbCred 1 afterSuccess
       else withMongoDBConn dbName dbHost dbPort Nothing 1 afterSuccess
