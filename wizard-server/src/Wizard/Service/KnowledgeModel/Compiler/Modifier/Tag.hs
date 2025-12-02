module Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddTagEvent Tag where
  createEntity event content =
    Tag
      { uuid = event.entityUuid
      , name = content.name
      , description = content.description
      , color = content.color
      , annotations = content.annotations
      }

instance EditEntity EditTagEvent Tag where
  editEntity event content entity =
    entity
      { name = applyValue entity.name content.name
      , description = applyValue entity.description content.description
      , color = applyValue entity.color content.color
      , annotations = applyValue entity.annotations content.annotations
      }
