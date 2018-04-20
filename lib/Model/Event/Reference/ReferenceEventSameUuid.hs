module Model.Event.Reference.ReferenceEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddReferenceEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid AddReferenceEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid AddReferenceEvent Reference where
  equalsUuid e ref = ref ^. uuid == e ^. referenceUuid

----------
-- Edit
----------
instance SameUuid EditReferenceEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid EditReferenceEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid EditReferenceEvent Reference where
  equalsUuid e ref = ref ^. uuid == e ^. referenceUuid

----------
-- Delete
----------
instance SameUuid DeleteReferenceEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

instance SameUuid DeleteReferenceEvent Question where
  equalsUuid e q = q ^. uuid == e ^. questionUuid

instance SameUuid DeleteReferenceEvent Reference where
  equalsUuid e ref = ref ^. uuid == e ^. referenceUuid
