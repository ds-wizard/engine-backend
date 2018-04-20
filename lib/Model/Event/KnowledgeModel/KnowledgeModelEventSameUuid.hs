module Model.Event.KnowledgeModel.KnowledgeModelEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddKnowledgeModelEvent KnowledgeModel where
  equalsUuid e km = km ^. uuid == e ^. kmUuid

----------
-- Edit
----------
instance SameUuid EditKnowledgeModelEvent KnowledgeModel where
  equalsUuid e km = km ^. uuid == e ^. kmUuid
