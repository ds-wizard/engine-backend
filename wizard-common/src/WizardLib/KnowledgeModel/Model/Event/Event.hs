module WizardLib.KnowledgeModel.Model.Event.Event where

import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent
import WizardLib.KnowledgeModel.Model.Event.Metric.MetricEvent
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEvent
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent
import WizardLib.KnowledgeModel.Model.Event.Reference.ReferenceEvent
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent

data Event
  = AddKnowledgeModelEvent' AddKnowledgeModelEvent
  | EditKnowledgeModelEvent' EditKnowledgeModelEvent
  | AddChapterEvent' AddChapterEvent
  | EditChapterEvent' EditChapterEvent
  | DeleteChapterEvent' DeleteChapterEvent
  | AddQuestionEvent' AddQuestionEvent
  | EditQuestionEvent' EditQuestionEvent
  | DeleteQuestionEvent' DeleteQuestionEvent
  | AddAnswerEvent' AddAnswerEvent
  | EditAnswerEvent' EditAnswerEvent
  | DeleteAnswerEvent' DeleteAnswerEvent
  | AddChoiceEvent' AddChoiceEvent
  | EditChoiceEvent' EditChoiceEvent
  | DeleteChoiceEvent' DeleteChoiceEvent
  | AddExpertEvent' AddExpertEvent
  | EditExpertEvent' EditExpertEvent
  | DeleteExpertEvent' DeleteExpertEvent
  | AddReferenceEvent' AddReferenceEvent
  | EditReferenceEvent' EditReferenceEvent
  | DeleteReferenceEvent' DeleteReferenceEvent
  | AddTagEvent' AddTagEvent
  | EditTagEvent' EditTagEvent
  | DeleteTagEvent' DeleteTagEvent
  | AddIntegrationEvent' AddIntegrationEvent
  | EditIntegrationEvent' EditIntegrationEvent
  | DeleteIntegrationEvent' DeleteIntegrationEvent
  | AddMetricEvent' AddMetricEvent
  | EditMetricEvent' EditMetricEvent
  | DeleteMetricEvent' DeleteMetricEvent
  | AddPhaseEvent' AddPhaseEvent
  | EditPhaseEvent' EditPhaseEvent
  | DeletePhaseEvent' DeletePhaseEvent
  | MoveQuestionEvent' MoveQuestionEvent
  | MoveAnswerEvent' MoveAnswerEvent
  | MoveChoiceEvent' MoveChoiceEvent
  | MoveExpertEvent' MoveExpertEvent
  | MoveReferenceEvent' MoveReferenceEvent
  deriving (Show, Eq, Generic)
