module Wizard.Service.KnowledgeModel.Compiler.Modifier.Choice where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddChoiceEvent Choice where
  createEntity event content =
    Choice
      { uuid = event.entityUuid
      , aLabel = content.aLabel
      , annotations = content.annotations
      }

instance EditEntity EditChoiceEvent Choice where
  editEntity event content entity =
    entity
      { aLabel = applyValue entity.aLabel content.aLabel
      , annotations = applyValue entity.annotations content.annotations
      }
