module Shared.Database.SqlConnection where

import Control.Lens ((^.))
import Data.ByteString.Char8 as BS
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres

import LensesConfig

createDatabaseConnectionPool serverConfig = do
  dbPool <-
    Pool.createPool
      (Postgres.connectPostgreSQL (BS.pack $ serverConfig ^. connectionString))
      Postgres.close
      1 -- stripes
      (realToFrac $ serverConfig ^. connectionTimeout)
      (serverConfig ^. maxConnections)
  verifyDatabaseConnectionPool dbPool
  return dbPool

verifyDatabaseConnectionPool dbPool = do
  let action conn = Postgres.query_ conn "select 2 + 2"
  Pool.withResource dbPool action :: IO [Postgres.Only Int]
  return ()
