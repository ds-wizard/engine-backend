module Database.Migration.Development.Metric.MetricMigration where

import Constant.Component
import Database.DAO.Metric.MetricDAO
import Database.Migration.Development.Metric.Data.Metrics
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Metric/Metric) started"
  deleteMetrics
  insertMetric metricF
  insertMetric metricA
  insertMetric metricI
  insertMetric metricR
  insertMetric metricG
  insertMetric metricO
  logInfo $ msg _CMP_MIGRATION "(Metric/Metric) ended"
