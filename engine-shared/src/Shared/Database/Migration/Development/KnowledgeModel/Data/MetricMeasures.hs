module Shared.Database.Migration.Development.KnowledgeModel.Data.MetricMeasures where

import Control.Lens
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- METRIC MEASURES
-- -----------------------------------------------------------------
metricMeasureF1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricF ^. uuid
    , _metricMeasureMeasure = 1.0
    , _metricMeasureWeight = 1.0
    , _metricMeasureAnnotations = M.empty
    }

metricMeasureA1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricA ^. uuid
    , _metricMeasureMeasure = 1.0
    , _metricMeasureWeight = 1.0
    , _metricMeasureAnnotations = M.empty
    }

metricMeasureI1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricI ^. uuid
    , _metricMeasureMeasure = 1.0
    , _metricMeasureWeight = 1.0
    , _metricMeasureAnnotations = M.empty
    }

metricMeasureI0_5 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricI ^. uuid
    , _metricMeasureMeasure = 0.5
    , _metricMeasureWeight = 0.5
    , _metricMeasureAnnotations = M.empty
    }

metricMeasureR1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricR ^. uuid
    , _metricMeasureMeasure = 1.0
    , _metricMeasureWeight = 1.0
    , _metricMeasureAnnotations = M.empty
    }

metricMeasureG1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricG ^. uuid
    , _metricMeasureMeasure = 1.0
    , _metricMeasureWeight = 1.0
    , _metricMeasureAnnotations = M.empty
    }

metricMeasureO1 =
  MetricMeasure
    { _metricMeasureMetricUuid = metricO ^. uuid
    , _metricMeasureMeasure = 1.0
    , _metricMeasureWeight = 1.0
    , _metricMeasureAnnotations = M.empty
    }
