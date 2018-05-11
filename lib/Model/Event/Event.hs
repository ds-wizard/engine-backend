module Model.Event.Event where

import Control.Lens ((^.))
import qualified Data.UUID as U
import GHC.Generics

import LensesConfig
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

isAddAction :: Event -> Bool
isAddAction (AddKnowledgeModelEvent' _) = True
isAddAction (AddChapterEvent' _) = True
isAddAction (AddQuestionEvent' _) = True
isAddAction (AddAnswerEvent' _) = True
isAddAction (AddAnswerItemTemplateQuestionEvent' _) = True
isAddAction (AddExpertEvent' _) = True
isAddAction (AddReferenceEvent' _) = True
isAddAction (AddFollowUpQuestionEvent' _) = True
isAddAction _ = False

isEditAction :: Event -> Bool
isEditAction (EditKnowledgeModelEvent' _) = True
isEditAction (EditChapterEvent' _) = True
isEditAction (EditQuestionEvent' _) = True
isEditAction (EditAnswerEvent' _) = True
isEditAction (EditAnswerItemTemplateQuestionEvent' _) = True
isEditAction (EditExpertEvent' _) = True
isEditAction (EditReferenceEvent' _) = True
isEditAction (EditFollowUpQuestionEvent' _) = True
isEditAction _ = False

isDeleteAction :: Event -> Bool
isDeleteAction (DeleteChapterEvent' _) = True
isDeleteAction (DeleteQuestionEvent' _) = True
isDeleteAction (DeleteAnswerEvent' _) = True
isDeleteAction (DeleteAnswerItemTemplateQuestionEvent' _) = True
isDeleteAction (DeleteExpertEvent' _) = True
isDeleteAction (DeleteReferenceEvent' _) = True
isDeleteAction (DeleteFollowUpQuestionEvent' _) = True
isDeleteAction _ = False

getEventUuid :: Event -> U.UUID
getEventUuid (AddKnowledgeModelEvent' event) = event ^. uuid
getEventUuid (EditKnowledgeModelEvent' event) = event ^. uuid
getEventUuid (AddChapterEvent' event) = event ^. uuid
getEventUuid (EditChapterEvent' event) = event ^. uuid
getEventUuid (DeleteChapterEvent' event) = event ^. uuid
getEventUuid (AddQuestionEvent' event) = event ^. uuid
getEventUuid (EditQuestionEvent' event) = event ^. uuid
getEventUuid (DeleteQuestionEvent' event) = event ^. uuid
getEventUuid (AddAnswerEvent' event) = event ^. uuid
getEventUuid (EditAnswerEvent' event) = event ^. uuid
getEventUuid (DeleteAnswerEvent' event) = event ^. uuid
getEventUuid (AddAnswerItemTemplateQuestionEvent' event) = event ^. uuid
getEventUuid (EditAnswerItemTemplateQuestionEvent' event) = event ^. uuid
getEventUuid (DeleteAnswerItemTemplateQuestionEvent' event) = event ^. uuid
getEventUuid (AddExpertEvent' event) = event ^. uuid
getEventUuid (EditExpertEvent' event) = event ^. uuid
getEventUuid (DeleteExpertEvent' event) = event ^. uuid
getEventUuid (AddReferenceEvent' event) = event ^. uuid
getEventUuid (EditReferenceEvent' event) = event ^. uuid
getEventUuid (DeleteReferenceEvent' event) = event ^. uuid
getEventUuid (AddFollowUpQuestionEvent' event) = event ^. uuid
getEventUuid (EditFollowUpQuestionEvent' event) = event ^. uuid
getEventUuid (DeleteFollowUpQuestionEvent' event) = event ^. uuid

getEventNodeUuid :: Event -> U.UUID
getEventNodeUuid (AddKnowledgeModelEvent' event) = event ^. kmUuid
getEventNodeUuid (EditKnowledgeModelEvent' event) = event ^. kmUuid
getEventNodeUuid (AddChapterEvent' event) = event ^. chapterUuid
getEventNodeUuid (EditChapterEvent' event) = event ^. chapterUuid
getEventNodeUuid (DeleteChapterEvent' event) = event ^. chapterUuid
getEventNodeUuid (AddQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (EditQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (DeleteQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (AddAnswerEvent' event) = event ^. answerUuid
getEventNodeUuid (EditAnswerEvent' event) = event ^. answerUuid
getEventNodeUuid (DeleteAnswerEvent' event) = event ^. answerUuid
getEventNodeUuid (AddAnswerItemTemplateQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (EditAnswerItemTemplateQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (DeleteAnswerItemTemplateQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (AddExpertEvent' event) = event ^. expertUuid
getEventNodeUuid (EditExpertEvent' event) = event ^. expertUuid
getEventNodeUuid (DeleteExpertEvent' event) = event ^. expertUuid
getEventNodeUuid (AddReferenceEvent' event) = event ^. referenceUuid
getEventNodeUuid (EditReferenceEvent' event) = event ^. referenceUuid
getEventNodeUuid (DeleteReferenceEvent' event) = event ^. referenceUuid
getEventNodeUuid (AddFollowUpQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (EditFollowUpQuestionEvent' event) = event ^. questionUuid
getEventNodeUuid (DeleteFollowUpQuestionEvent' event) = event ^. questionUuid
