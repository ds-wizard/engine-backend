module Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddAnswerEvent Answer where
  createEntity event content =
    Answer
      { uuid = event.entityUuid
      , aLabel = content.aLabel
      , advice = content.advice
      , annotations = content.annotations
      , followUpUuids = []
      , metricMeasures = content.metricMeasures
      }

instance EditEntity EditAnswerEvent Answer where
  editEntity event content entity =
    entity
      { aLabel = applyValue entity.aLabel content.aLabel
      , advice = applyValue entity.advice content.advice
      , annotations = applyValue entity.annotations content.annotations
      , followUpUuids = applyValue entity.followUpUuids content.followUpUuids
      , metricMeasures = applyValue entity.metricMeasures content.metricMeasures
      }

deleteMetricReference :: KnowledgeModelEvent -> Answer -> Answer
deleteMetricReference event ans =
  let updatedMetrics = filter (\mm -> mm.metricUuid /= event.entityUuid) ans.metricMeasures
   in ans {metricMeasures = updatedMetrics}
