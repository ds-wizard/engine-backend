module Wizard.Database.Migration.Production.Migration_0004_metrics_init.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Wizard.Database.Migration.Production.Migration_0004_metrics_init.Data.Metrics

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 4, mmName = "Metrics Init", mmDescription = ""}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  insertF dbPool
  insertA dbPool
  insertI dbPool
  insertR dbPool
  insertG dbPool
  insertO dbPool
  return Nothing

insertF dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "metrics" (metricF now)
  runMongoDBPoolDef action dbPool

insertA dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "metrics" (metricA now)
  runMongoDBPoolDef action dbPool

insertI dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "metrics" (metricI now)
  runMongoDBPoolDef action dbPool

insertR dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "metrics" (metricR now)
  runMongoDBPoolDef action dbPool

insertG dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "metrics" (metricG now)
  runMongoDBPoolDef action dbPool

insertO dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "metrics" (metricO now)
  runMongoDBPoolDef action dbPool
