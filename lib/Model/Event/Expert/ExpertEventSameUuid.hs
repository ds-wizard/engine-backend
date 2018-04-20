module Model.Event.Expert.ExpertEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.Expert.ExpertEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddExpertEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid AddExpertEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid AddExpertEvent Expert where
  equalsUuid e exp = exp ^. uuid == e ^. expertUuid

----------
-- Edit
----------
instance SameUuid EditExpertEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid EditExpertEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid EditExpertEvent Expert where
  equalsUuid e exp = exp ^. uuid == e ^. expertUuid

----------
-- Delete
----------
instance SameUuid DeleteExpertEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid DeleteExpertEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid DeleteExpertEvent Expert where
  equalsUuid e exp = exp ^. uuid == e ^. expertUuid
