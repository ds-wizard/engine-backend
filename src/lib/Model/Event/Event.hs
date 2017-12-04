module Model.Event.Event where

import Control.Lens ((^.))
import qualified Data.UUID as U
import GHC.Generics

import Common.Types
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Common
import Model.Event.Expert.AddExpertEvent
import Model.Event.Expert.DeleteExpertEvent
import Model.Event.Expert.EditExpertEvent
import Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.DeleteQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Reference.AddReferenceEvent
import Model.Event.Reference.DeleteReferenceEvent
import Model.Event.Reference.EditReferenceEvent

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
isAddAction (AddExpertEvent' _) = True
isAddAction (AddReferenceEvent' _) = True
isAddAction (AddFollowUpQuestionEvent' _) = True
isAddAction _ = False

isEditAction :: Event -> Bool
isEditAction (EditKnowledgeModelEvent' _) = True
isEditAction (EditChapterEvent' _) = True
isEditAction (EditQuestionEvent' _) = True
isEditAction (EditAnswerEvent' _) = True
isEditAction (EditExpertEvent' _) = True
isEditAction (EditReferenceEvent' _) = True
isEditAction (EditFollowUpQuestionEvent' _) = True
isEditAction _ = False

isDeleteAction :: Event -> Bool
isDeleteAction (DeleteChapterEvent' _) = True
isDeleteAction (DeleteQuestionEvent' _) = True
isDeleteAction (DeleteAnswerEvent' _) = True
isDeleteAction (DeleteExpertEvent' _) = True
isDeleteAction (DeleteReferenceEvent' _) = True
isDeleteAction (DeleteFollowUpQuestionEvent' _) = True
isDeleteAction _ = False

getEventUuid :: Event -> U.UUID
getEventUuid (AddKnowledgeModelEvent' event) = event ^. akmUuid
getEventUuid (EditKnowledgeModelEvent' event) = event ^. ekmUuid
getEventUuid (AddChapterEvent' event) = event ^. achUuid
getEventUuid (EditChapterEvent' event) = event ^. echUuid
getEventUuid (DeleteChapterEvent' event) = event ^. dchUuid
getEventUuid (AddQuestionEvent' event) = event ^. aqUuid
getEventUuid (EditQuestionEvent' event) = event ^. eqUuid
getEventUuid (DeleteQuestionEvent' event) = event ^. dqUuid
getEventUuid (AddAnswerEvent' event) = event ^. aansUuid
getEventUuid (EditAnswerEvent' event) = event ^. eansUuid
getEventUuid (DeleteAnswerEvent' event) = event ^. dansUuid
getEventUuid (AddExpertEvent' event) = event ^. aexpUuid
getEventUuid (EditExpertEvent' event) = event ^. eexpUuid
getEventUuid (DeleteExpertEvent' event) = event ^. dexpUuid
getEventUuid (AddReferenceEvent' event) = event ^. arefUuid
getEventUuid (EditReferenceEvent' event) = event ^. erefUuid
getEventUuid (DeleteReferenceEvent' event) = event ^. drefUuid
getEventUuid (AddFollowUpQuestionEvent' event) = event ^. afuqUuid
getEventUuid (EditFollowUpQuestionEvent' event) = event ^. efuqUuid
getEventUuid (DeleteFollowUpQuestionEvent' event) = event ^. dfuqUuid

getEventNodeUuid :: Event -> U.UUID
getEventNodeUuid (AddKnowledgeModelEvent' event) = event ^. akmKmUuid
getEventNodeUuid (EditKnowledgeModelEvent' event) = event ^. ekmKmUuid
getEventNodeUuid (AddChapterEvent' event) = event ^. achChapterUuid
getEventNodeUuid (EditChapterEvent' event) = event ^. echChapterUuid
getEventNodeUuid (DeleteChapterEvent' event) = event ^. dchChapterUuid
getEventNodeUuid (AddQuestionEvent' event) = event ^. aqQuestionUuid
getEventNodeUuid (EditQuestionEvent' event) = event ^. eqQuestionUuid
getEventNodeUuid (DeleteQuestionEvent' event) = event ^. dqQuestionUuid
getEventNodeUuid (AddAnswerEvent' event) = event ^. aansAnswerUuid
getEventNodeUuid (EditAnswerEvent' event) = event ^. eansAnswerUuid
getEventNodeUuid (DeleteAnswerEvent' event) = event ^. dansAnswerUuid
getEventNodeUuid (AddExpertEvent' event) = event ^. aexpExpertUuid
getEventNodeUuid (EditExpertEvent' event) = event ^. eexpExpertUuid
getEventNodeUuid (DeleteExpertEvent' event) = event ^. dexpExpertUuid
getEventNodeUuid (AddReferenceEvent' event) = event ^. arefReferenceUuid
getEventNodeUuid (EditReferenceEvent' event) = event ^. erefReferenceUuid
getEventNodeUuid (DeleteReferenceEvent' event) = event ^. drefReferenceUuid
getEventNodeUuid (AddFollowUpQuestionEvent' event) = event ^. afuqQuestionUuid
getEventNodeUuid (EditFollowUpQuestionEvent' event) = event ^. efuqQuestionUuid
getEventNodeUuid (DeleteFollowUpQuestionEvent' event) = event ^. dfuqQuestionUuid
