module Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- METRIC MEASURES
-- -----------------------------------------------------------------
metricMeasureF1 =
  MetricMeasure {metricUuid = metricF.uuid, measure = 1.0, weight = 1.0}

metricMeasureA1 =
  MetricMeasure {metricUuid = metricA.uuid, measure = 1.0, weight = 1.0}

metricMeasureI1 =
  MetricMeasure {metricUuid = metricI.uuid, measure = 1.0, weight = 1.0}

metricMeasureI0_5 =
  MetricMeasure {metricUuid = metricI.uuid, measure = 0.5, weight = 0.5}

metricMeasureR1 =
  MetricMeasure {metricUuid = metricR.uuid, measure = 1.0, weight = 1.0}

metricMeasureG1 =
  MetricMeasure {metricUuid = metricG.uuid, measure = 1.0, weight = 1.0}

metricMeasureO1 =
  MetricMeasure {metricUuid = metricO.uuid, measure = 1.0, weight = 1.0}
