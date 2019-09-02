module Database.Migration.Development.KnowledgeModel.Data.MetricMeasures where

import Control.Lens

import Database.Migration.Development.Metric.Data.Metrics
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- METRIC MEASURES
-- -----------------------------------------------------------------
metricMeasureF1 =
  MetricMeasure {_metricMeasureMetricUuid = metricF ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureA1 =
  MetricMeasure {_metricMeasureMetricUuid = metricA ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureI1 =
  MetricMeasure {_metricMeasureMetricUuid = metricI ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureR1 =
  MetricMeasure {_metricMeasureMetricUuid = metricR ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureG1 =
  MetricMeasure {_metricMeasureMetricUuid = metricG ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureO1 =
  MetricMeasure {_metricMeasureMetricUuid = metricO ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}
