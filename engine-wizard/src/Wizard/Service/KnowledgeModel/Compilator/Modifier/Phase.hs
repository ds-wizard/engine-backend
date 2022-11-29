module Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase where

import Shared.Model.Event.Phase.PhaseEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddPhaseEvent Phase where
  createEntity event =
    Phase
      { uuid = event.entityUuid
      , title = event.title
      , description = event.description
      , annotations = event.annotations
      }

instance EditEntity EditPhaseEvent Phase where
  editEntity event entity =
    entity
      { title = applyValue entity.title event.title
      , description = applyValue entity.description event.description
      , annotations = applyValue entity.annotations event.annotations
      }
