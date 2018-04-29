module Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddAnswerItemTemplateQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid AddAnswerItemTemplateQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

----------
-- Edit
----------
instance SameUuid EditAnswerItemTemplateQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid EditAnswerItemTemplateQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

----------
-- Delete
----------
instance SameUuid DeleteAnswerItemTemplateQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid DeleteAnswerItemTemplateQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid
