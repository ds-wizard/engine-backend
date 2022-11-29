module Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer where

import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddAnswerEvent Answer where
  createEntity event =
    Answer
      { uuid = event.entityUuid
      , aLabel = event.aLabel
      , advice = event.advice
      , annotations = event.annotations
      , followUpUuids = []
      , metricMeasures = event.metricMeasures
      }

instance EditEntity EditAnswerEvent Answer where
  editEntity event entity =
    entity
      { aLabel = applyValue entity.aLabel event.aLabel
      , advice = applyValue entity.advice event.advice
      , annotations = applyValue entity.annotations event.annotations
      , followUpUuids = applyValue entity.followUpUuids event.followUpUuids
      , metricMeasures = applyValue entity.metricMeasures event.metricMeasures
      }

deleteMetricReference :: DeleteMetricEvent -> Answer -> Answer
deleteMetricReference event ans =
  let updatedMetrics = filter (\mm -> mm.metricUuid /= event.entityUuid) ans.metricMeasures
   in ans {metricMeasures = updatedMetrics}
