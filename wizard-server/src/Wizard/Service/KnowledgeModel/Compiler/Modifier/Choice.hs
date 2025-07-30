module Wizard.Service.KnowledgeModel.Compiler.Modifier.Choice where

import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddChoiceEvent Choice where
  createEntity event =
    Choice {uuid = event.entityUuid, aLabel = event.aLabel, annotations = event.annotations}

instance EditEntity EditChoiceEvent Choice where
  editEntity event entity =
    entity
      { aLabel = applyValue entity.aLabel event.aLabel
      , annotations = applyValue entity.annotations event.annotations
      }
