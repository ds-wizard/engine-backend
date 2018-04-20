module Model.Event.FollowUpQuestion.FollowUpQuestionEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid AddFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid AddFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. uuid == e ^. answerUuid

----------
-- Edit
----------
instance SameUuid EditFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid EditFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid EditFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. uuid == e ^. answerUuid

----------
-- Delete
----------
instance SameUuid DeleteFollowUpQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid DeleteFollowUpQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid DeleteFollowUpQuestionEvent Answer where
  equalsUuid e ans = ans ^. uuid == e ^. answerUuid
