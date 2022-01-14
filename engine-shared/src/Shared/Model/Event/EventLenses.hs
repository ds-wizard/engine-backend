module Shared.Model.Event.EventLenses
  ( module Shared.Model.Common.Lens
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Common.Lens
import Shared.Model.Event.Answer.AnswerEventLenses ()
import Shared.Model.Event.Chapter.ChapterEventLenses ()
import Shared.Model.Event.Choice.ChoiceEventLenses ()
import Shared.Model.Event.Event
import Shared.Model.Event.Expert.ExpertEventLenses ()
import Shared.Model.Event.Integration.IntegrationEventLenses ()
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEventLenses ()
import Shared.Model.Event.Metric.MetricEventLenses ()
import Shared.Model.Event.Move.MoveEventLenses ()
import Shared.Model.Event.Phase.PhaseEventLenses ()
import Shared.Model.Event.Question.QuestionEventLenses ()
import Shared.Model.Event.Reference.ReferenceEventLenses ()
import Shared.Model.Event.Tag.TagEventLenses ()

instance HasUuid' Event where
  uuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> U.UUID
      get (AddKnowledgeModelEvent' entity) = entity ^. uuid'
      get (EditKnowledgeModelEvent' entity) = entity ^. uuid'
      get (AddChapterEvent' entity) = entity ^. uuid'
      get (EditChapterEvent' entity) = entity ^. uuid'
      get (DeleteChapterEvent' entity) = entity ^. uuid'
      get (AddQuestionEvent' entity) = entity ^. uuid'
      get (EditQuestionEvent' entity) = entity ^. uuid'
      get (DeleteQuestionEvent' entity) = entity ^. uuid'
      get (AddAnswerEvent' entity) = entity ^. uuid'
      get (EditAnswerEvent' entity) = entity ^. uuid'
      get (DeleteAnswerEvent' entity) = entity ^. uuid'
      get (AddChoiceEvent' entity) = entity ^. uuid'
      get (EditChoiceEvent' entity) = entity ^. uuid'
      get (DeleteChoiceEvent' entity) = entity ^. uuid'
      get (AddExpertEvent' entity) = entity ^. uuid'
      get (EditExpertEvent' entity) = entity ^. uuid'
      get (DeleteExpertEvent' entity) = entity ^. uuid'
      get (AddReferenceEvent' entity) = entity ^. uuid'
      get (EditReferenceEvent' entity) = entity ^. uuid'
      get (DeleteReferenceEvent' entity) = entity ^. uuid'
      get (AddMetricEvent' entity) = entity ^. uuid'
      get (EditMetricEvent' entity) = entity ^. uuid'
      get (DeleteMetricEvent' entity) = entity ^. uuid'
      get (AddPhaseEvent' entity) = entity ^. uuid'
      get (EditPhaseEvent' entity) = entity ^. uuid'
      get (DeletePhaseEvent' entity) = entity ^. uuid'
      get (AddTagEvent' entity) = entity ^. uuid'
      get (EditTagEvent' entity) = entity ^. uuid'
      get (DeleteTagEvent' entity) = entity ^. uuid'
      get (AddIntegrationEvent' entity) = entity ^. uuid'
      get (EditIntegrationEvent' entity) = entity ^. uuid'
      get (DeleteIntegrationEvent' entity) = entity ^. uuid'
      get (MoveQuestionEvent' entity) = entity ^. uuid'
      get (MoveAnswerEvent' entity) = entity ^. uuid'
      get (MoveChoiceEvent' entity) = entity ^. uuid'
      get (MoveExpertEvent' entity) = entity ^. uuid'
      get (MoveReferenceEvent' entity) = entity ^. uuid'
      set :: Event -> U.UUID -> Event
      set (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ entity & uuid' .~ newValue
      set (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ entity & uuid' .~ newValue
      set (AddChapterEvent' entity) newValue = AddChapterEvent' $ entity & uuid' .~ newValue
      set (EditChapterEvent' entity) newValue = EditChapterEvent' $ entity & uuid' .~ newValue
      set (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ entity & uuid' .~ newValue
      set (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ entity & uuid' .~ newValue
      set (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ entity & uuid' .~ newValue
      set (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ entity & uuid' .~ newValue
      set (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ entity & uuid' .~ newValue
      set (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ entity & uuid' .~ newValue
      set (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ entity & uuid' .~ newValue
      set (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ entity & uuid' .~ newValue
      set (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ entity & uuid' .~ newValue
      set (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ entity & uuid' .~ newValue
      set (AddExpertEvent' entity) newValue = AddExpertEvent' $ entity & uuid' .~ newValue
      set (EditExpertEvent' entity) newValue = EditExpertEvent' $ entity & uuid' .~ newValue
      set (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ entity & uuid' .~ newValue
      set (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ entity & uuid' .~ newValue
      set (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ entity & uuid' .~ newValue
      set (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ entity & uuid' .~ newValue
      set (AddMetricEvent' entity) newValue = AddMetricEvent' $ entity & uuid' .~ newValue
      set (EditMetricEvent' entity) newValue = EditMetricEvent' $ entity & uuid' .~ newValue
      set (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ entity & uuid' .~ newValue
      set (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ entity & uuid' .~ newValue
      set (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ entity & uuid' .~ newValue
      set (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ entity & uuid' .~ newValue
      set (AddTagEvent' entity) newValue = AddTagEvent' $ entity & uuid' .~ newValue
      set (EditTagEvent' entity) newValue = EditTagEvent' $ entity & uuid' .~ newValue
      set (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ entity & uuid' .~ newValue
      set (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ entity & uuid' .~ newValue
      set (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ entity & uuid' .~ newValue
      set (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ entity & uuid' .~ newValue
      set (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ entity & uuid' .~ newValue
      set (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ entity & uuid' .~ newValue
      set (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ entity & uuid' .~ newValue
      set (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ entity & uuid' .~ newValue
      set (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ entity & uuid' .~ newValue

instance HasParentUuid' Event where
  parentUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> U.UUID
      get (AddKnowledgeModelEvent' entity) = entity ^. parentUuid'
      get (EditKnowledgeModelEvent' entity) = entity ^. parentUuid'
      get (AddChapterEvent' entity) = entity ^. parentUuid'
      get (EditChapterEvent' entity) = entity ^. parentUuid'
      get (DeleteChapterEvent' entity) = entity ^. parentUuid'
      get (AddQuestionEvent' entity) = entity ^. parentUuid'
      get (EditQuestionEvent' entity) = entity ^. parentUuid'
      get (DeleteQuestionEvent' entity) = entity ^. parentUuid'
      get (AddAnswerEvent' entity) = entity ^. parentUuid'
      get (EditAnswerEvent' entity) = entity ^. parentUuid'
      get (DeleteAnswerEvent' entity) = entity ^. parentUuid'
      get (AddChoiceEvent' entity) = entity ^. parentUuid'
      get (EditChoiceEvent' entity) = entity ^. parentUuid'
      get (DeleteChoiceEvent' entity) = entity ^. parentUuid'
      get (AddExpertEvent' entity) = entity ^. parentUuid'
      get (EditExpertEvent' entity) = entity ^. parentUuid'
      get (DeleteExpertEvent' entity) = entity ^. parentUuid'
      get (AddReferenceEvent' entity) = entity ^. parentUuid'
      get (EditReferenceEvent' entity) = entity ^. parentUuid'
      get (DeleteReferenceEvent' entity) = entity ^. parentUuid'
      get (AddMetricEvent' entity) = entity ^. parentUuid'
      get (EditMetricEvent' entity) = entity ^. parentUuid'
      get (DeleteMetricEvent' entity) = entity ^. parentUuid'
      get (AddPhaseEvent' entity) = entity ^. parentUuid'
      get (EditPhaseEvent' entity) = entity ^. parentUuid'
      get (DeletePhaseEvent' entity) = entity ^. parentUuid'
      get (AddTagEvent' entity) = entity ^. parentUuid'
      get (EditTagEvent' entity) = entity ^. parentUuid'
      get (DeleteTagEvent' entity) = entity ^. parentUuid'
      get (AddIntegrationEvent' entity) = entity ^. parentUuid'
      get (EditIntegrationEvent' entity) = entity ^. parentUuid'
      get (DeleteIntegrationEvent' entity) = entity ^. parentUuid'
      get (MoveQuestionEvent' entity) = entity ^. parentUuid'
      get (MoveAnswerEvent' entity) = entity ^. parentUuid'
      get (MoveChoiceEvent' entity) = entity ^. parentUuid'
      get (MoveExpertEvent' entity) = entity ^. parentUuid'
      get (MoveReferenceEvent' entity) = entity ^. parentUuid'
      set :: Event -> U.UUID -> Event
      set (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ entity & parentUuid' .~ newValue
      set (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ entity & parentUuid' .~ newValue
      set (AddChapterEvent' entity) newValue = AddChapterEvent' $ entity & parentUuid' .~ newValue
      set (EditChapterEvent' entity) newValue = EditChapterEvent' $ entity & parentUuid' .~ newValue
      set (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ entity & parentUuid' .~ newValue
      set (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ entity & parentUuid' .~ newValue
      set (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ entity & parentUuid' .~ newValue
      set (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ entity & parentUuid' .~ newValue
      set (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ entity & parentUuid' .~ newValue
      set (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ entity & parentUuid' .~ newValue
      set (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ entity & parentUuid' .~ newValue
      set (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ entity & parentUuid' .~ newValue
      set (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ entity & parentUuid' .~ newValue
      set (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ entity & parentUuid' .~ newValue
      set (AddExpertEvent' entity) newValue = AddExpertEvent' $ entity & parentUuid' .~ newValue
      set (EditExpertEvent' entity) newValue = EditExpertEvent' $ entity & parentUuid' .~ newValue
      set (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ entity & parentUuid' .~ newValue
      set (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ entity & parentUuid' .~ newValue
      set (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ entity & parentUuid' .~ newValue
      set (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ entity & parentUuid' .~ newValue
      set (AddMetricEvent' entity) newValue = AddMetricEvent' $ entity & parentUuid' .~ newValue
      set (EditMetricEvent' entity) newValue = EditMetricEvent' $ entity & parentUuid' .~ newValue
      set (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ entity & parentUuid' .~ newValue
      set (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ entity & parentUuid' .~ newValue
      set (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ entity & parentUuid' .~ newValue
      set (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ entity & parentUuid' .~ newValue
      set (AddTagEvent' entity) newValue = AddTagEvent' $ entity & parentUuid' .~ newValue
      set (EditTagEvent' entity) newValue = EditTagEvent' $ entity & parentUuid' .~ newValue
      set (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ entity & parentUuid' .~ newValue
      set (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ entity & parentUuid' .~ newValue
      set (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ entity & parentUuid' .~ newValue
      set (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ entity & parentUuid' .~ newValue
      set (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ entity & parentUuid' .~ newValue
      set (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ entity & parentUuid' .~ newValue
      set (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ entity & parentUuid' .~ newValue
      set (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ entity & parentUuid' .~ newValue
      set (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ entity & parentUuid' .~ newValue

instance HasEntityUuid' Event where
  entityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> U.UUID
      get (AddKnowledgeModelEvent' entity) = entity ^. entityUuid'
      get (EditKnowledgeModelEvent' entity) = entity ^. entityUuid'
      get (AddChapterEvent' entity) = entity ^. entityUuid'
      get (EditChapterEvent' entity) = entity ^. entityUuid'
      get (DeleteChapterEvent' entity) = entity ^. entityUuid'
      get (AddQuestionEvent' entity) = entity ^. entityUuid'
      get (EditQuestionEvent' entity) = entity ^. entityUuid'
      get (DeleteQuestionEvent' entity) = entity ^. entityUuid'
      get (AddAnswerEvent' entity) = entity ^. entityUuid'
      get (EditAnswerEvent' entity) = entity ^. entityUuid'
      get (DeleteAnswerEvent' entity) = entity ^. entityUuid'
      get (AddChoiceEvent' entity) = entity ^. entityUuid'
      get (EditChoiceEvent' entity) = entity ^. entityUuid'
      get (DeleteChoiceEvent' entity) = entity ^. entityUuid'
      get (AddExpertEvent' entity) = entity ^. entityUuid'
      get (EditExpertEvent' entity) = entity ^. entityUuid'
      get (DeleteExpertEvent' entity) = entity ^. entityUuid'
      get (AddReferenceEvent' entity) = entity ^. entityUuid'
      get (EditReferenceEvent' entity) = entity ^. entityUuid'
      get (DeleteReferenceEvent' entity) = entity ^. entityUuid'
      get (AddMetricEvent' entity) = entity ^. entityUuid'
      get (EditMetricEvent' entity) = entity ^. entityUuid'
      get (DeleteMetricEvent' entity) = entity ^. entityUuid'
      get (AddPhaseEvent' entity) = entity ^. entityUuid'
      get (EditPhaseEvent' entity) = entity ^. entityUuid'
      get (DeletePhaseEvent' entity) = entity ^. entityUuid'
      get (AddTagEvent' entity) = entity ^. entityUuid'
      get (EditTagEvent' entity) = entity ^. entityUuid'
      get (DeleteTagEvent' entity) = entity ^. entityUuid'
      get (AddIntegrationEvent' entity) = entity ^. entityUuid'
      get (EditIntegrationEvent' entity) = entity ^. entityUuid'
      get (DeleteIntegrationEvent' entity) = entity ^. entityUuid'
      get (MoveQuestionEvent' entity) = entity ^. entityUuid'
      get (MoveAnswerEvent' entity) = entity ^. entityUuid'
      get (MoveChoiceEvent' entity) = entity ^. entityUuid'
      get (MoveExpertEvent' entity) = entity ^. entityUuid'
      get (MoveReferenceEvent' entity) = entity ^. entityUuid'
      set :: Event -> U.UUID -> Event
      set (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ entity & entityUuid' .~ newValue
      set (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ entity & entityUuid' .~ newValue
      set (AddChapterEvent' entity) newValue = AddChapterEvent' $ entity & entityUuid' .~ newValue
      set (EditChapterEvent' entity) newValue = EditChapterEvent' $ entity & entityUuid' .~ newValue
      set (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ entity & entityUuid' .~ newValue
      set (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ entity & entityUuid' .~ newValue
      set (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ entity & entityUuid' .~ newValue
      set (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ entity & entityUuid' .~ newValue
      set (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ entity & entityUuid' .~ newValue
      set (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ entity & entityUuid' .~ newValue
      set (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ entity & entityUuid' .~ newValue
      set (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ entity & entityUuid' .~ newValue
      set (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ entity & entityUuid' .~ newValue
      set (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ entity & entityUuid' .~ newValue
      set (AddExpertEvent' entity) newValue = AddExpertEvent' $ entity & entityUuid' .~ newValue
      set (EditExpertEvent' entity) newValue = EditExpertEvent' $ entity & entityUuid' .~ newValue
      set (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ entity & entityUuid' .~ newValue
      set (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ entity & entityUuid' .~ newValue
      set (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ entity & entityUuid' .~ newValue
      set (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ entity & entityUuid' .~ newValue
      set (AddMetricEvent' entity) newValue = AddMetricEvent' $ entity & entityUuid' .~ newValue
      set (EditMetricEvent' entity) newValue = EditMetricEvent' $ entity & entityUuid' .~ newValue
      set (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ entity & entityUuid' .~ newValue
      set (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ entity & entityUuid' .~ newValue
      set (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ entity & entityUuid' .~ newValue
      set (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ entity & entityUuid' .~ newValue
      set (AddTagEvent' entity) newValue = AddTagEvent' $ entity & entityUuid' .~ newValue
      set (EditTagEvent' entity) newValue = EditTagEvent' $ entity & entityUuid' .~ newValue
      set (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ entity & entityUuid' .~ newValue
      set (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ entity & entityUuid' .~ newValue
      set (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ entity & entityUuid' .~ newValue
      set (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ entity & entityUuid' .~ newValue
      set (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ entity & entityUuid' .~ newValue
      set (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ entity & entityUuid' .~ newValue
      set (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ entity & entityUuid' .~ newValue
      set (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ entity & entityUuid' .~ newValue
      set (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ entity & entityUuid' .~ newValue

instance HasCreatedAt' Event where
  createdAt' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: Event -> UTCTime
      get (AddKnowledgeModelEvent' entity) = entity ^. createdAt'
      get (EditKnowledgeModelEvent' entity) = entity ^. createdAt'
      get (AddChapterEvent' entity) = entity ^. createdAt'
      get (EditChapterEvent' entity) = entity ^. createdAt'
      get (DeleteChapterEvent' entity) = entity ^. createdAt'
      get (AddQuestionEvent' entity) = entity ^. createdAt'
      get (EditQuestionEvent' entity) = entity ^. createdAt'
      get (DeleteQuestionEvent' entity) = entity ^. createdAt'
      get (AddAnswerEvent' entity) = entity ^. createdAt'
      get (EditAnswerEvent' entity) = entity ^. createdAt'
      get (DeleteAnswerEvent' entity) = entity ^. createdAt'
      get (AddChoiceEvent' entity) = entity ^. createdAt'
      get (EditChoiceEvent' entity) = entity ^. createdAt'
      get (DeleteChoiceEvent' entity) = entity ^. createdAt'
      get (AddExpertEvent' entity) = entity ^. createdAt'
      get (EditExpertEvent' entity) = entity ^. createdAt'
      get (DeleteExpertEvent' entity) = entity ^. createdAt'
      get (AddReferenceEvent' entity) = entity ^. createdAt'
      get (EditReferenceEvent' entity) = entity ^. createdAt'
      get (DeleteReferenceEvent' entity) = entity ^. createdAt'
      get (AddMetricEvent' entity) = entity ^. createdAt'
      get (EditMetricEvent' entity) = entity ^. createdAt'
      get (DeleteMetricEvent' entity) = entity ^. createdAt'
      get (AddPhaseEvent' entity) = entity ^. createdAt'
      get (EditPhaseEvent' entity) = entity ^. createdAt'
      get (DeletePhaseEvent' entity) = entity ^. createdAt'
      get (AddTagEvent' entity) = entity ^. createdAt'
      get (EditTagEvent' entity) = entity ^. createdAt'
      get (DeleteTagEvent' entity) = entity ^. createdAt'
      get (AddIntegrationEvent' entity) = entity ^. createdAt'
      get (EditIntegrationEvent' entity) = entity ^. createdAt'
      get (DeleteIntegrationEvent' entity) = entity ^. createdAt'
      get (MoveQuestionEvent' entity) = entity ^. createdAt'
      get (MoveAnswerEvent' entity) = entity ^. createdAt'
      get (MoveChoiceEvent' entity) = entity ^. createdAt'
      get (MoveExpertEvent' entity) = entity ^. createdAt'
      get (MoveReferenceEvent' entity) = entity ^. createdAt'
      set :: Event -> UTCTime -> Event
      set (AddKnowledgeModelEvent' entity) newValue = AddKnowledgeModelEvent' $ entity & createdAt' .~ newValue
      set (EditKnowledgeModelEvent' entity) newValue = EditKnowledgeModelEvent' $ entity & createdAt' .~ newValue
      set (AddChapterEvent' entity) newValue = AddChapterEvent' $ entity & createdAt' .~ newValue
      set (EditChapterEvent' entity) newValue = EditChapterEvent' $ entity & createdAt' .~ newValue
      set (DeleteChapterEvent' entity) newValue = DeleteChapterEvent' $ entity & createdAt' .~ newValue
      set (AddQuestionEvent' entity) newValue = AddQuestionEvent' $ entity & createdAt' .~ newValue
      set (EditQuestionEvent' entity) newValue = EditQuestionEvent' $ entity & createdAt' .~ newValue
      set (DeleteQuestionEvent' entity) newValue = DeleteQuestionEvent' $ entity & createdAt' .~ newValue
      set (AddAnswerEvent' entity) newValue = AddAnswerEvent' $ entity & createdAt' .~ newValue
      set (EditAnswerEvent' entity) newValue = EditAnswerEvent' $ entity & createdAt' .~ newValue
      set (DeleteAnswerEvent' entity) newValue = DeleteAnswerEvent' $ entity & createdAt' .~ newValue
      set (AddChoiceEvent' entity) newValue = AddChoiceEvent' $ entity & createdAt' .~ newValue
      set (EditChoiceEvent' entity) newValue = EditChoiceEvent' $ entity & createdAt' .~ newValue
      set (DeleteChoiceEvent' entity) newValue = DeleteChoiceEvent' $ entity & createdAt' .~ newValue
      set (AddExpertEvent' entity) newValue = AddExpertEvent' $ entity & createdAt' .~ newValue
      set (EditExpertEvent' entity) newValue = EditExpertEvent' $ entity & createdAt' .~ newValue
      set (DeleteExpertEvent' entity) newValue = DeleteExpertEvent' $ entity & createdAt' .~ newValue
      set (AddReferenceEvent' entity) newValue = AddReferenceEvent' $ entity & createdAt' .~ newValue
      set (EditReferenceEvent' entity) newValue = EditReferenceEvent' $ entity & createdAt' .~ newValue
      set (DeleteReferenceEvent' entity) newValue = DeleteReferenceEvent' $ entity & createdAt' .~ newValue
      set (AddMetricEvent' entity) newValue = AddMetricEvent' $ entity & createdAt' .~ newValue
      set (EditMetricEvent' entity) newValue = EditMetricEvent' $ entity & createdAt' .~ newValue
      set (DeleteMetricEvent' entity) newValue = DeleteMetricEvent' $ entity & createdAt' .~ newValue
      set (AddPhaseEvent' entity) newValue = AddPhaseEvent' $ entity & createdAt' .~ newValue
      set (EditPhaseEvent' entity) newValue = EditPhaseEvent' $ entity & createdAt' .~ newValue
      set (DeletePhaseEvent' entity) newValue = DeletePhaseEvent' $ entity & createdAt' .~ newValue
      set (AddTagEvent' entity) newValue = AddTagEvent' $ entity & createdAt' .~ newValue
      set (EditTagEvent' entity) newValue = EditTagEvent' $ entity & createdAt' .~ newValue
      set (DeleteTagEvent' entity) newValue = DeleteTagEvent' $ entity & createdAt' .~ newValue
      set (AddIntegrationEvent' entity) newValue = AddIntegrationEvent' $ entity & createdAt' .~ newValue
      set (EditIntegrationEvent' entity) newValue = EditIntegrationEvent' $ entity & createdAt' .~ newValue
      set (DeleteIntegrationEvent' entity) newValue = DeleteIntegrationEvent' $ entity & createdAt' .~ newValue
      set (MoveQuestionEvent' entity) newValue = MoveQuestionEvent' $ entity & createdAt' .~ newValue
      set (MoveAnswerEvent' entity) newValue = MoveAnswerEvent' $ entity & createdAt' .~ newValue
      set (MoveChoiceEvent' entity) newValue = MoveChoiceEvent' $ entity & createdAt' .~ newValue
      set (MoveExpertEvent' entity) newValue = MoveExpertEvent' $ entity & createdAt' .~ newValue
      set (MoveReferenceEvent' entity) newValue = MoveReferenceEvent' $ entity & createdAt' .~ newValue
