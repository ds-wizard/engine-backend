module Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag where

import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddTagEvent Tag where
  createEntity event =
    Tag
      { uuid = event.entityUuid
      , name = event.name
      , description = event.description
      , color = event.color
      , annotations = event.annotations
      }

instance EditEntity EditTagEvent Tag where
  editEntity event entity =
    entity
      { name = applyValue entity.name event.name
      , description = applyValue entity.description event.description
      , color = applyValue entity.color event.color
      , annotations = applyValue entity.annotations event.annotations
      }
