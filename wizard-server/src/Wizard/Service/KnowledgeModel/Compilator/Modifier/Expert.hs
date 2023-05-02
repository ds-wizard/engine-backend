module Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert where

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Expert.ExpertEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddExpertEvent Expert where
  createEntity event =
    Expert
      { uuid = event.entityUuid
      , name = event.name
      , email = event.email
      , annotations = event.annotations
      }

instance EditEntity EditExpertEvent Expert where
  editEntity event entity =
    entity
      { name = applyValue entity.name event.name
      , email = applyValue entity.email event.email
      , annotations = applyValue entity.annotations event.annotations
      }
