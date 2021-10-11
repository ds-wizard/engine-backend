module Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddMetricEvent Metric where
  createEntity e =
    Metric
      { _metricUuid = e ^. entityUuid
      , _metricTitle = e ^. title
      , _metricAbbreviation = e ^. abbreviation
      , _metricDescription = e ^. description
      , _metricAnnotations = e ^. annotations
      }

instance EditEntity EditMetricEvent Metric where
  editEntity e = applyAnnotations . applyDescription . applyAbbreviation . applyTitle
    where
      applyTitle m = applyValue (e ^. title) m title
      applyAbbreviation m = applyValue (e ^. abbreviation) m abbreviation
      applyDescription m = applyValue (e ^. description) m description
      applyAnnotations m = applyValue (e ^. annotations) m annotations
