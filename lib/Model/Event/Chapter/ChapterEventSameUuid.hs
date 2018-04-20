module Model.Event.Chapter.ChapterEventSameUuid where

import Control.Lens ((^.))

import LensesConfig
import Model.Common
import Model.Event.Chapter.ChapterEvent
import Model.KnowledgeModel.KnowledgeModel

----------
-- Add
----------
instance SameUuid AddChapterEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

----------
-- Edit
----------
instance SameUuid EditChapterEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid

----------
-- Delete
----------
instance SameUuid DeleteChapterEvent KnowledgeModel where
  equalsUuid e ch = ch ^. uuid == e ^. kmUuid

instance SameUuid DeleteChapterEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. chapterUuid
