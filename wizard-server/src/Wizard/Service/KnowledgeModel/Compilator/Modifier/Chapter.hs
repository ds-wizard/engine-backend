module Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter where

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
