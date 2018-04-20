module Model.Event.Answer.AnswerEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.Answer.AnswerEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddAnswerEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid AddAnswerEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid AddAnswerEvent Answer where
  equalsUuid e ans = ans ^. uuid == e ^. answerUuid

----------
-- Edit
----------
instance SameUuid EditAnswerEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid EditAnswerEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid EditAnswerEvent Answer where
  equalsUuid e ans = ans ^. uuid == e ^. answerUuid

----------
-- Delete
----------
instance SameUuid DeleteAnswerEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid DeleteAnswerEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid DeleteAnswerEvent Answer where
  equalsUuid e ans = ans ^. uuid == e ^. answerUuid
