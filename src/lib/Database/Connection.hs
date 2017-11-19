module Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.Persist.MongoDB (withMongoDBConn)
import Network

import Common.DSPConfig
import Database.Migration.Migration

createDBConn dspConfig afterSuccess =
  let appConfigDatabase = dspConfig ^. dspcfgDatabaseConfig
      dbHost = appConfigDatabase ^. acdbHost
      dbPort =
        PortNumber (fromInteger (appConfigDatabase ^. acdbPort) :: PortNumber) :: PortID
      dbName = pack (appConfigDatabase ^. acdbDatabaseName)
  in withMongoDBConn dbName dbHost dbPort Nothing 10100 afterSuccess
