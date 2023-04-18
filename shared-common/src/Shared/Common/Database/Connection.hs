module Shared.Common.Database.Connection where

import Data.ByteString.Char8 as BS
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres

createDatabaseConnectionPool serverConfig = do
  dbPool <-
    Pool.createPool
      (Postgres.connectPostgreSQL (BS.pack serverConfig.connectionString))
      Postgres.close
      serverConfig.stripeSize
      (realToFrac serverConfig.connectionTimeout)
      serverConfig.maxConnections
  verifyDatabaseConnectionPool dbPool
  return dbPool

verifyDatabaseConnectionPool dbPool = do
  let action conn = Postgres.query_ conn "SELECT 2 + 2"
  Pool.withResource dbPool action :: IO [Postgres.Only Int]
  return ()
