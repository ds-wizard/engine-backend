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
      }

instance EditEntity EditMetricEvent Metric where
  editEntity e = applyDescription . applyAbbreviation . applyTitle
    where
      applyTitle exp = applyValue (e ^. title) exp title
      applyAbbreviation exp = applyValue (e ^. abbreviation) exp abbreviation
      applyDescription exp = applyValue (e ^. description) exp description
