module Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter where

import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddChapterEvent Chapter where
  createEntity event =
    Chapter
      { uuid = event.entityUuid
      , title = event.title
      , text = event.text
      , annotations = event.annotations
      , questionUuids = []
      }

instance EditEntity EditChapterEvent Chapter where
  editEntity event entity =
    entity
      { title = applyValue entity.title event.title
      , text = applyValue entity.text event.text
      , annotations = applyValue entity.annotations event.annotations
      , questionUuids = applyValue entity.questionUuids event.questionUuids
      }
