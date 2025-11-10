module Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.KnowledgeModel.Util.Hashable ()

data KnowledgeModelEvent = KnowledgeModelEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , content :: KnowledgeModelEventData
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data KnowledgeModelEventData
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
  | AddResourceCollectionEvent' AddResourceCollectionEvent
  | EditResourceCollectionEvent' EditResourceCollectionEvent
  | DeleteResourceCollectionEvent' DeleteResourceCollectionEvent
  | AddResourcePageEvent' AddResourcePageEvent
  | EditResourcePageEvent' EditResourcePageEvent
  | DeleteResourcePageEvent' DeleteResourcePageEvent
  | MoveQuestionEvent' MoveQuestionEvent
  | MoveAnswerEvent' MoveAnswerEvent
  | MoveChoiceEvent' MoveChoiceEvent
  | MoveExpertEvent' MoveExpertEvent
  | MoveReferenceEvent' MoveReferenceEvent
  deriving (Show, Eq, Generic)

instance Hashable KnowledgeModelEvent

instance Hashable KnowledgeModelEventData
