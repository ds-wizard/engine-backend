module Wizard.Service.KnowledgeModel.Compilator.Modifier.Choice where

import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddChoiceEvent Choice where
  createEntity event =
    Choice {uuid = event.entityUuid, aLabel = event.aLabel, annotations = event.annotations}

instance EditEntity EditChoiceEvent Choice where
  editEntity event entity =
    entity
      { aLabel = applyValue entity.aLabel event.aLabel
      , annotations = applyValue entity.annotations event.annotations
      }
