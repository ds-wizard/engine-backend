module Wizard.Database.Migration.Development.Metric.MetricMigration where

import Shared.Database.Migration.Development.Metric.Data.Metrics
import Wizard.Constant.Component
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Metric/Metric) started"
  deleteMetrics
  insertMetric metricF
  insertMetric metricA
  insertMetric metricI
  insertMetric metricR
  insertMetric metricG
  insertMetric metricO
  logInfo _CMP_MIGRATION "(Metric/Metric) ended"
