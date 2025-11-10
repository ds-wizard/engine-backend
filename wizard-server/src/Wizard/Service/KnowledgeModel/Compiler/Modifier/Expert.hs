module Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddExpertEvent Expert where
  createEntity event content =
    Expert
      { uuid = event.entityUuid
      , name = content.name
      , email = content.email
      , annotations = content.annotations
      }

instance EditEntity EditExpertEvent Expert where
  editEntity event content entity =
    entity
      { name = applyValue entity.name content.name
      , email = applyValue entity.email content.email
      , annotations = applyValue entity.annotations content.annotations
      }
