module Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.Persist.MongoDB (withMongoDBConn)
import Network

import Common.DSWConfig
import Database.Migration.Migration

createDBConn dswConfig afterSuccess =
  let appConfigDatabase = dswConfig ^. dswcfgDatabaseConfig
      dbHost = appConfigDatabase ^. acdbHost
      dbPort = PortNumber (fromInteger (appConfigDatabase ^. acdbPort) :: PortNumber) :: PortID
      dbName = pack (appConfigDatabase ^. acdbDatabaseName)
  in withMongoDBConn dbName dbHost dbPort Nothing 1 afterSuccess
