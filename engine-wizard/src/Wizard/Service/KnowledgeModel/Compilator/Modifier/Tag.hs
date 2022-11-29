module Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag where

import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

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
