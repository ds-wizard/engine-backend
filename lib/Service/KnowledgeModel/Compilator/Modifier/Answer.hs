module Service.KnowledgeModel.Compilator.Modifier.Answer where

import Control.Lens ((^.))

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddAnswerEvent Answer where
  createEntity e =
    Answer
    { _answerUuid = e ^. entityUuid
    , _answerLabel = e ^. label
    , _answerAdvice = e ^. advice
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = e ^. metricMeasures
    }

instance EditEntity EditAnswerEvent Answer where
  editEntity e = applyMetricMeasures . applyFollowUpUuids . applyAdvice . applyLabel
    where
      applyLabel ans = applyValue (e ^. label) ans label
      applyAdvice ans = applyValue (e ^. advice) ans advice
      applyFollowUpUuids ans = applyValue (e ^. followUpUuids) ans followUpUuids
      applyMetricMeasures ans = applyValue (e ^. metricMeasures) ans metricMeasures
