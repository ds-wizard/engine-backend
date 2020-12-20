module Shared.Model.Event.Event where

import GHC.Generics

import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent

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
  | MoveQuestionEvent' MoveQuestionEvent
  | MoveAnswerEvent' MoveAnswerEvent
  | MoveChoiceEvent' MoveChoiceEvent
  | MoveExpertEvent' MoveExpertEvent
  | MoveReferenceEvent' MoveReferenceEvent
  deriving (Show, Eq, Generic)
