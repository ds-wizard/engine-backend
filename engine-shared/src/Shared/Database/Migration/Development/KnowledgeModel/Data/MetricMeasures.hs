module Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures where

import Control.Lens

import LensesConfig
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- METRIC MEASURES
-- -----------------------------------------------------------------
metricMeasureF1 =
  MetricMeasure {_metricMeasureMetricUuid = metricF ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureA1 =
  MetricMeasure {_metricMeasureMetricUuid = metricA ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureI1 =
  MetricMeasure {_metricMeasureMetricUuid = metricI ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureI0_5 =
  MetricMeasure {_metricMeasureMetricUuid = metricI ^. uuid, _metricMeasureMeasure = 0.5, _metricMeasureWeight = 0.5}

metricMeasureR1 =
  MetricMeasure {_metricMeasureMetricUuid = metricR ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureG1 =
  MetricMeasure {_metricMeasureMetricUuid = metricG ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}

metricMeasureO1 =
  MetricMeasure {_metricMeasureMetricUuid = metricO ^. uuid, _metricMeasureMeasure = 1.0, _metricMeasureWeight = 1.0}
