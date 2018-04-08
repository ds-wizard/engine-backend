module Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.Persist.MongoDB (withMongoDBConn)
import Network

import Database.Migration.Migration
import LensesConfig
import Model.Config.DSWConfig

createDBConn dswConfig afterSuccess =
  let appConfigDatabase = dswConfig ^. databaseConfig
      dbHost = appConfigDatabase ^. host
      dbPort = PortNumber (fromInteger (appConfigDatabase ^. port) :: PortNumber) :: PortID
      dbName = pack (appConfigDatabase ^. databaseName)
  in withMongoDBConn dbName dbHost dbPort Nothing 1 afterSuccess
