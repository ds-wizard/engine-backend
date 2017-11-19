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
import Model.Event.Common
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

getTargetUuid :: Event -> U.UUID
getTargetUuid (AddKnowledgeModelEvent' event) = event ^. akmKmUuid
getTargetUuid (EditKnowledgeModelEvent' event) = event ^. ekmKmUuid
getTargetUuid (AddChapterEvent' event) = event ^. achChapterUuid
getTargetUuid (EditChapterEvent' event) = event ^. echChapterUuid
getTargetUuid (DeleteChapterEvent' event) = event ^. dchChapterUuid
getTargetUuid (AddQuestionEvent' event) = event ^. aqQuestionUuid
getTargetUuid (EditQuestionEvent' event) = event ^. eqQuestionUuid
getTargetUuid (DeleteQuestionEvent' event) = event ^. dqQuestionUuid
getTargetUuid (AddAnswerEvent' event) = event ^. aansAnswerUuid
getTargetUuid (EditAnswerEvent' event) = event ^. eansAnswerUuid
getTargetUuid (DeleteAnswerEvent' event) = event ^. dansAnswerUuid
getTargetUuid (AddExpertEvent' event) = event ^. aexpExpertUuid
getTargetUuid (EditExpertEvent' event) = event ^. eexpExpertUuid
getTargetUuid (DeleteExpertEvent' event) = event ^. dexpExpertUuid
getTargetUuid (AddReferenceEvent' event) = event ^. arefReferenceUuid
getTargetUuid (EditReferenceEvent' event) = event ^. erefReferenceUuid
getTargetUuid (DeleteReferenceEvent' event) = event ^. drefReferenceUuid
getTargetUuid (AddFollowUpQuestionEvent' event) = event ^. afuqQuestionUuid
getTargetUuid (EditFollowUpQuestionEvent' event) = event ^. efuqQuestionUuid
getTargetUuid (DeleteFollowUpQuestionEvent' event) = event ^. dfuqQuestionUuid
