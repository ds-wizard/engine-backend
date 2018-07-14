module Database.Migration.Development.Metric.MetricMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.Metric.MetricDAO
import Database.Migration.Development.Metric.Data.Metrics

runMigration = do
  $(logInfo) "MIGRATION (Metric/Metric): started"
  deleteMetrics
  insertMetric metricF
  insertMetric metricA
  insertMetric metricI
  insertMetric metricR
  insertMetric metricG
  insertMetric metricO
  $(logInfo) "MIGRATION (Metric/Metric): ended"
