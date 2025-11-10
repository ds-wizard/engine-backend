module Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddPhaseEvent Phase where
  createEntity event content =
    Phase
      { uuid = event.entityUuid
      , title = content.title
      , description = content.description
      , annotations = content.annotations
      }

instance EditEntity EditPhaseEvent Phase where
  editEntity event content entity =
    entity
      { title = applyValue entity.title content.title
      , description = applyValue entity.description content.description
      , annotations = applyValue entity.annotations content.annotations
      }
