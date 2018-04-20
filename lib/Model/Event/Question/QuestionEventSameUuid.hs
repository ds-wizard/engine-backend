module Model.Event.Question.QuestionEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.Question.QuestionEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid AddQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

----------
-- Edit
----------
instance SameUuid EditQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid EditQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

----------
-- Delete
----------
instance SameUuid DeleteQuestionEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid DeleteQuestionEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid
