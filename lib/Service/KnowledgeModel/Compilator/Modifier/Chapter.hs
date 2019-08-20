module Service.KnowledgeModel.Compilator.Modifier.Chapter where

import Control.Lens ((^.))

import LensesConfig
import Model.Event.Chapter.ChapterEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddChapterEvent Chapter where
  createEntity e =
    Chapter
    {_chapterUuid = e ^. entityUuid, _chapterTitle = e ^. title, _chapterText = e ^. text, _chapterQuestionUuids = []}

instance EditEntity EditChapterEvent Chapter where
  editEntity e = applyQuestionUuids . applyText . applyTitle
    where
      applyTitle ch = applyValue (e ^. title) ch title
      applyText ch = applyValue (e ^. text) ch text
      applyQuestionUuids ch = applyValue (e ^. questionUuids) ch questionUuids
