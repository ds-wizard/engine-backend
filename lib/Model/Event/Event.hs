module Model.Event.Event where

import GHC.Generics

import Model.Event.Answer.AnswerEvent
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent

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
  | AddAnswerItemTemplateQuestionEvent' AddAnswerItemTemplateQuestionEvent
  | EditAnswerItemTemplateQuestionEvent' EditAnswerItemTemplateQuestionEvent
  | DeleteAnswerItemTemplateQuestionEvent' DeleteAnswerItemTemplateQuestionEvent
  | AddExpertEvent' AddExpertEvent
  | EditExpertEvent' EditExpertEvent
  | DeleteExpertEvent' DeleteExpertEvent
  | AddReferenceEvent' AddReferenceEvent
  | EditReferenceEvent' EditReferenceEvent
  | DeleteReferenceEvent' DeleteReferenceEvent
  | AddFollowUpQuestionEvent' AddFollowUpQuestionEvent
  | EditFollowUpQuestionEvent' EditFollowUpQuestionEvent
  | DeleteFollowUpQuestionEvent' DeleteFollowUpQuestionEvent
  deriving (Show, Eq, Generic)
