module Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric where

import Shared.Model.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddMetricEvent Metric where
  createEntity event =
    Metric
      { uuid = event.entityUuid
      , title = event.title
      , abbreviation = event.abbreviation
      , description = event.description
      , annotations = event.annotations
      }

instance EditEntity EditMetricEvent Metric where
  editEntity event entity =
    entity
      { title = applyValue entity.title event.title
      , abbreviation = applyValue entity.abbreviation event.abbreviation
      , description = applyValue entity.description event.description
      , annotations = applyValue entity.annotations event.annotations
      }
