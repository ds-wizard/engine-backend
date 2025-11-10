module Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddMetricEvent Metric where
  createEntity event content =
    Metric
      { uuid = event.entityUuid
      , title = content.title
      , abbreviation = content.abbreviation
      , description = content.description
      , annotations = content.annotations
      }

instance EditEntity EditMetricEvent Metric where
  editEntity event content entity =
    entity
      { title = applyValue entity.title content.title
      , abbreviation = applyValue entity.abbreviation content.abbreviation
      , description = applyValue entity.description content.description
      , annotations = applyValue entity.annotations content.annotations
      }
