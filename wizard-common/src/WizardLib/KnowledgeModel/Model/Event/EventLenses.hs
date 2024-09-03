module WizardLib.KnowledgeModel.Model.Event.EventLenses (
  module Shared.Common.Model.Common.Lens,
) where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Metric.MetricEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Reference.ReferenceEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEventLenses ()

instance HasUuid' Event where
  getUuid (AddKnowledgeModelEvent' entity) = getUuid entity
  getUuid (EditKnowledgeModelEvent' entity) = getUuid entity
  getUuid (AddChapterEvent' entity) = getUuid entity
  getUuid (EditChapterEvent' entity) = getUuid entity
  getUuid (DeleteChapterEvent' entity) = getUuid entity
  getUuid (AddQuestionEvent' entity) = getUuid entity
  getUuid (EditQuestionEvent' entity) = getUuid entity
  getUuid (DeleteQuestionEvent' entity) = getUuid entity
  getUuid (AddAnswerEvent' entity) = getUuid entity
  getUuid (EditAnswerEvent' entity) = getUuid entity
  getUuid (DeleteAnswerEvent' entity) = getUuid entity
  getUuid (AddChoiceEvent' entity) = getUuid entity
  getUuid (EditChoiceEvent' entity) = getUuid entity
  getUuid (DeleteChoiceEvent' entity) = getUuid entity
  getUuid (AddExpertEvent' entity) = getUuid entity
  getUuid (EditExpertEvent' entity) = getUuid entity
  getUuid (DeleteExpertEvent' entity) = getUuid entity
  getUuid (AddReferenceEvent' entity) = getUuid entity
  getUuid (EditReferenceEvent' entity) = getUuid entity
  getUuid (DeleteReferenceEvent' entity) = getUuid entity
  getUuid (AddMetricEvent' entity) = getUuid entity
  getUuid (EditMetricEvent' entity) = getUuid entity
  getUuid (DeleteMetricEvent' entity) = getUuid entity
  getUuid (AddPhaseEvent' entity) = getUuid entity
  getUuid (EditPhaseEvent' entity) = getUuid entity
  getUuid (DeletePhaseEvent' entity) = getUuid entity
  getUuid (AddTagEvent' entity) = getUuid entity
  getUuid (EditTagEvent' entity) = getUuid entity
  getUuid (DeleteTagEvent' entity) = getUuid entity
  getUuid (AddIntegrationEvent' entity) = getUuid entity
  getUuid (EditIntegrationEvent' entity) = getUuid entity
  getUuid (DeleteIntegrationEvent' entity) = getUuid entity
  getUuid (AddResourceCollectionEvent' entity) = getUuid entity
  getUuid (EditResourceCollectionEvent' entity) = getUuid entity
  getUuid (DeleteResourceCollectionEvent' entity) = getUuid entity
  getUuid (AddResourcePageEvent' entity) = getUuid entity
  getUuid (EditResourcePageEvent' entity) = getUuid entity
  getUuid (DeleteResourcePageEvent' entity) = getUuid entity
  getUuid (MoveQuestionEvent' entity) = getUuid entity
  getUuid (MoveAnswerEvent' entity) = getUuid entity
  getUuid (MoveChoiceEvent' entity) = getUuid entity
  getUuid (MoveExpertEvent' entity) = getUuid entity
  getUuid (MoveReferenceEvent' entity) = getUuid entity
  setUuid (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ setUuid entity newValue
  setUuid (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ setUuid entity newValue
  setUuid (AddChapterEvent' entity) newValue = AddChapterEvent' $ setUuid entity newValue
  setUuid (EditChapterEvent' entity) newValue = EditChapterEvent' $ setUuid entity newValue
  setUuid (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ setUuid entity newValue
  setUuid (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ setUuid entity newValue
  setUuid (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ setUuid entity newValue
  setUuid (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ setUuid entity newValue
  setUuid (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ setUuid entity newValue
  setUuid (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ setUuid entity newValue
  setUuid (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ setUuid entity newValue
  setUuid (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ setUuid entity newValue
  setUuid (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ setUuid entity newValue
  setUuid (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ setUuid entity newValue
  setUuid (AddExpertEvent' entity) newValue = AddExpertEvent' $ setUuid entity newValue
  setUuid (EditExpertEvent' entity) newValue = EditExpertEvent' $ setUuid entity newValue
  setUuid (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ setUuid entity newValue
  setUuid (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ setUuid entity newValue
  setUuid (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ setUuid entity newValue
  setUuid (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ setUuid entity newValue
  setUuid (AddMetricEvent' entity) newValue = AddMetricEvent' $ setUuid entity newValue
  setUuid (EditMetricEvent' entity) newValue = EditMetricEvent' $ setUuid entity newValue
  setUuid (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ setUuid entity newValue
  setUuid (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ setUuid entity newValue
  setUuid (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ setUuid entity newValue
  setUuid (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ setUuid entity newValue
  setUuid (AddTagEvent' entity) newValue = AddTagEvent' $ setUuid entity newValue
  setUuid (EditTagEvent' entity) newValue = EditTagEvent' $ setUuid entity newValue
  setUuid (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ setUuid entity newValue
  setUuid (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ setUuid entity newValue
  setUuid (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ setUuid entity newValue
  setUuid (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ setUuid entity newValue
  setUuid (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ setUuid entity newValue
  setUuid (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ setUuid entity newValue
  setUuid (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ setUuid entity newValue
  setUuid (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ setUuid entity newValue
  setUuid (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ setUuid entity newValue

instance HasParentUuid' Event where
  getParentUuid (AddKnowledgeModelEvent' entity) = getParentUuid entity
  getParentUuid (EditKnowledgeModelEvent' entity) = getParentUuid entity
  getParentUuid (AddChapterEvent' entity) = getParentUuid entity
  getParentUuid (EditChapterEvent' entity) = getParentUuid entity
  getParentUuid (DeleteChapterEvent' entity) = getParentUuid entity
  getParentUuid (AddQuestionEvent' entity) = getParentUuid entity
  getParentUuid (EditQuestionEvent' entity) = getParentUuid entity
  getParentUuid (DeleteQuestionEvent' entity) = getParentUuid entity
  getParentUuid (AddAnswerEvent' entity) = getParentUuid entity
  getParentUuid (EditAnswerEvent' entity) = getParentUuid entity
  getParentUuid (DeleteAnswerEvent' entity) = getParentUuid entity
  getParentUuid (AddChoiceEvent' entity) = getParentUuid entity
  getParentUuid (EditChoiceEvent' entity) = getParentUuid entity
  getParentUuid (DeleteChoiceEvent' entity) = getParentUuid entity
  getParentUuid (AddExpertEvent' entity) = getParentUuid entity
  getParentUuid (EditExpertEvent' entity) = getParentUuid entity
  getParentUuid (DeleteExpertEvent' entity) = getParentUuid entity
  getParentUuid (AddReferenceEvent' entity) = getParentUuid entity
  getParentUuid (EditReferenceEvent' entity) = getParentUuid entity
  getParentUuid (DeleteReferenceEvent' entity) = getParentUuid entity
  getParentUuid (AddMetricEvent' entity) = getParentUuid entity
  getParentUuid (EditMetricEvent' entity) = getParentUuid entity
  getParentUuid (DeleteMetricEvent' entity) = getParentUuid entity
  getParentUuid (AddPhaseEvent' entity) = getParentUuid entity
  getParentUuid (EditPhaseEvent' entity) = getParentUuid entity
  getParentUuid (DeletePhaseEvent' entity) = getParentUuid entity
  getParentUuid (AddTagEvent' entity) = getParentUuid entity
  getParentUuid (EditTagEvent' entity) = getParentUuid entity
  getParentUuid (DeleteTagEvent' entity) = getParentUuid entity
  getParentUuid (AddIntegrationEvent' entity) = getParentUuid entity
  getParentUuid (EditIntegrationEvent' entity) = getParentUuid entity
  getParentUuid (DeleteIntegrationEvent' entity) = getParentUuid entity
  getParentUuid (AddResourceCollectionEvent' entity) = getParentUuid entity
  getParentUuid (EditResourceCollectionEvent' entity) = getParentUuid entity
  getParentUuid (DeleteResourceCollectionEvent' entity) = getParentUuid entity
  getParentUuid (AddResourcePageEvent' entity) = getParentUuid entity
  getParentUuid (EditResourcePageEvent' entity) = getParentUuid entity
  getParentUuid (DeleteResourcePageEvent' entity) = getParentUuid entity
  getParentUuid (MoveQuestionEvent' entity) = getParentUuid entity
  getParentUuid (MoveAnswerEvent' entity) = getParentUuid entity
  getParentUuid (MoveChoiceEvent' entity) = getParentUuid entity
  getParentUuid (MoveExpertEvent' entity) = getParentUuid entity
  getParentUuid (MoveReferenceEvent' entity) = getParentUuid entity
  setParentUuid (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ setParentUuid entity newValue
  setParentUuid (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ setParentUuid entity newValue
  setParentUuid (AddChapterEvent' entity) newValue = AddChapterEvent' $ setParentUuid entity newValue
  setParentUuid (EditChapterEvent' entity) newValue = EditChapterEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ setParentUuid entity newValue
  setParentUuid (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ setParentUuid entity newValue
  setParentUuid (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ setParentUuid entity newValue
  setParentUuid (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ setParentUuid entity newValue
  setParentUuid (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ setParentUuid entity newValue
  setParentUuid (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ setParentUuid entity newValue
  setParentUuid (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ setParentUuid entity newValue
  setParentUuid (AddExpertEvent' entity) newValue = AddExpertEvent' $ setParentUuid entity newValue
  setParentUuid (EditExpertEvent' entity) newValue = EditExpertEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ setParentUuid entity newValue
  setParentUuid (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ setParentUuid entity newValue
  setParentUuid (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ setParentUuid entity newValue
  setParentUuid (AddMetricEvent' entity) newValue = AddMetricEvent' $ setParentUuid entity newValue
  setParentUuid (EditMetricEvent' entity) newValue = EditMetricEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ setParentUuid entity newValue
  setParentUuid (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ setParentUuid entity newValue
  setParentUuid (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ setParentUuid entity newValue
  setParentUuid (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ setParentUuid entity newValue
  setParentUuid (AddTagEvent' entity) newValue = AddTagEvent' $ setParentUuid entity newValue
  setParentUuid (EditTagEvent' entity) newValue = EditTagEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ setParentUuid entity newValue
  setParentUuid (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ setParentUuid entity newValue
  setParentUuid (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ setParentUuid entity newValue
  setParentUuid (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ setParentUuid entity newValue
  setParentUuid (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ setParentUuid entity newValue
  setParentUuid (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ setParentUuid entity newValue
  setParentUuid (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ setParentUuid entity newValue
  setParentUuid (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ setParentUuid entity newValue
  setParentUuid (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ setParentUuid entity newValue

instance HasEntityUuid' Event where
  getEntityUuid (AddKnowledgeModelEvent' entity) = getEntityUuid entity
  getEntityUuid (EditKnowledgeModelEvent' entity) = getEntityUuid entity
  getEntityUuid (AddChapterEvent' entity) = getEntityUuid entity
  getEntityUuid (EditChapterEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteChapterEvent' entity) = getEntityUuid entity
  getEntityUuid (AddQuestionEvent' entity) = getEntityUuid entity
  getEntityUuid (EditQuestionEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteQuestionEvent' entity) = getEntityUuid entity
  getEntityUuid (AddAnswerEvent' entity) = getEntityUuid entity
  getEntityUuid (EditAnswerEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteAnswerEvent' entity) = getEntityUuid entity
  getEntityUuid (AddChoiceEvent' entity) = getEntityUuid entity
  getEntityUuid (EditChoiceEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteChoiceEvent' entity) = getEntityUuid entity
  getEntityUuid (AddExpertEvent' entity) = getEntityUuid entity
  getEntityUuid (EditExpertEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteExpertEvent' entity) = getEntityUuid entity
  getEntityUuid (AddReferenceEvent' entity) = getEntityUuid entity
  getEntityUuid (EditReferenceEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteReferenceEvent' entity) = getEntityUuid entity
  getEntityUuid (AddMetricEvent' entity) = getEntityUuid entity
  getEntityUuid (EditMetricEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteMetricEvent' entity) = getEntityUuid entity
  getEntityUuid (AddPhaseEvent' entity) = getEntityUuid entity
  getEntityUuid (EditPhaseEvent' entity) = getEntityUuid entity
  getEntityUuid (DeletePhaseEvent' entity) = getEntityUuid entity
  getEntityUuid (AddTagEvent' entity) = getEntityUuid entity
  getEntityUuid (EditTagEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteTagEvent' entity) = getEntityUuid entity
  getEntityUuid (AddIntegrationEvent' entity) = getEntityUuid entity
  getEntityUuid (EditIntegrationEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteIntegrationEvent' entity) = getEntityUuid entity
  getEntityUuid (AddResourceCollectionEvent' entity) = getEntityUuid entity
  getEntityUuid (EditResourceCollectionEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteResourceCollectionEvent' entity) = getEntityUuid entity
  getEntityUuid (AddResourcePageEvent' entity) = getEntityUuid entity
  getEntityUuid (EditResourcePageEvent' entity) = getEntityUuid entity
  getEntityUuid (DeleteResourcePageEvent' entity) = getEntityUuid entity
  getEntityUuid (MoveQuestionEvent' entity) = getEntityUuid entity
  getEntityUuid (MoveAnswerEvent' entity) = getEntityUuid entity
  getEntityUuid (MoveChoiceEvent' entity) = getEntityUuid entity
  getEntityUuid (MoveExpertEvent' entity) = getEntityUuid entity
  getEntityUuid (MoveReferenceEvent' entity) = getEntityUuid entity
  setEntityUuid (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddChapterEvent' entity) newValue = AddChapterEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditChapterEvent' entity) newValue = EditChapterEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddExpertEvent' entity) newValue = AddExpertEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditExpertEvent' entity) newValue = EditExpertEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddMetricEvent' entity) newValue = AddMetricEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditMetricEvent' entity) newValue = EditMetricEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddTagEvent' entity) newValue = AddTagEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditTagEvent' entity) newValue = EditTagEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ setEntityUuid entity newValue
  setEntityUuid (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ setEntityUuid entity newValue
  setEntityUuid (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ setEntityUuid entity newValue
  setEntityUuid (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ setEntityUuid entity newValue
  setEntityUuid (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ setEntityUuid entity newValue
  setEntityUuid (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ setEntityUuid entity newValue
  setEntityUuid (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ setEntityUuid entity newValue
  setEntityUuid (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ setEntityUuid entity newValue
  setEntityUuid (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ setEntityUuid entity newValue

instance HasCreatedAt' Event where
  getCreatedAt (AddKnowledgeModelEvent' entity) = getCreatedAt entity
  getCreatedAt (EditKnowledgeModelEvent' entity) = getCreatedAt entity
  getCreatedAt (AddChapterEvent' entity) = getCreatedAt entity
  getCreatedAt (EditChapterEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteChapterEvent' entity) = getCreatedAt entity
  getCreatedAt (AddQuestionEvent' entity) = getCreatedAt entity
  getCreatedAt (EditQuestionEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteQuestionEvent' entity) = getCreatedAt entity
  getCreatedAt (AddAnswerEvent' entity) = getCreatedAt entity
  getCreatedAt (EditAnswerEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteAnswerEvent' entity) = getCreatedAt entity
  getCreatedAt (AddChoiceEvent' entity) = getCreatedAt entity
  getCreatedAt (EditChoiceEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteChoiceEvent' entity) = getCreatedAt entity
  getCreatedAt (AddExpertEvent' entity) = getCreatedAt entity
  getCreatedAt (EditExpertEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteExpertEvent' entity) = getCreatedAt entity
  getCreatedAt (AddReferenceEvent' entity) = getCreatedAt entity
  getCreatedAt (EditReferenceEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteReferenceEvent' entity) = getCreatedAt entity
  getCreatedAt (AddMetricEvent' entity) = getCreatedAt entity
  getCreatedAt (EditMetricEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteMetricEvent' entity) = getCreatedAt entity
  getCreatedAt (AddPhaseEvent' entity) = getCreatedAt entity
  getCreatedAt (EditPhaseEvent' entity) = getCreatedAt entity
  getCreatedAt (DeletePhaseEvent' entity) = getCreatedAt entity
  getCreatedAt (AddTagEvent' entity) = getCreatedAt entity
  getCreatedAt (EditTagEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteTagEvent' entity) = getCreatedAt entity
  getCreatedAt (AddIntegrationEvent' entity) = getCreatedAt entity
  getCreatedAt (EditIntegrationEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteIntegrationEvent' entity) = getCreatedAt entity
  getCreatedAt (AddResourceCollectionEvent' entity) = getCreatedAt entity
  getCreatedAt (EditResourceCollectionEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteResourceCollectionEvent' entity) = getCreatedAt entity
  getCreatedAt (AddResourcePageEvent' entity) = getCreatedAt entity
  getCreatedAt (EditResourcePageEvent' entity) = getCreatedAt entity
  getCreatedAt (DeleteResourcePageEvent' entity) = getCreatedAt entity
  getCreatedAt (MoveQuestionEvent' entity) = getCreatedAt entity
  getCreatedAt (MoveAnswerEvent' entity) = getCreatedAt entity
  getCreatedAt (MoveChoiceEvent' entity) = getCreatedAt entity
  getCreatedAt (MoveExpertEvent' entity) = getCreatedAt entity
  getCreatedAt (MoveReferenceEvent' entity) = getCreatedAt entity
  setCreatedAt (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddChapterEvent' entity) newValue = AddChapterEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditChapterEvent' entity) newValue = EditChapterEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddExpertEvent' entity) newValue = AddExpertEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditExpertEvent' entity) newValue = EditExpertEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddMetricEvent' entity) newValue = AddMetricEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditMetricEvent' entity) newValue = EditMetricEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddTagEvent' entity) newValue = AddTagEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditTagEvent' entity) newValue = EditTagEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ setCreatedAt entity newValue
  setCreatedAt (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ setCreatedAt entity newValue
  setCreatedAt (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ setCreatedAt entity newValue
  setCreatedAt (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ setCreatedAt entity newValue
  setCreatedAt (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ setCreatedAt entity newValue
  setCreatedAt (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ setCreatedAt entity newValue
  setCreatedAt (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ setCreatedAt entity newValue
  setCreatedAt (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ setCreatedAt entity newValue
  setCreatedAt (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ setCreatedAt entity newValue
