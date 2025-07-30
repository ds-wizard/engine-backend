module Wizard.Service.KnowledgeModel.Compiler.Modifier.Resource where

import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddResourceCollectionEvent ResourceCollection where
  createEntity event =
    ResourceCollection
      { uuid = event.entityUuid
      , title = event.title
      , resourcePageUuids = []
      , annotations = event.annotations
      }

instance EditEntity EditResourceCollectionEvent ResourceCollection where
  editEntity event entity =
    entity
      { title = applyValue entity.title event.title
      , resourcePageUuids = applyValue entity.resourcePageUuids event.resourcePageUuids
      , annotations = applyValue entity.annotations event.annotations
      }

-- --------------------------------------------
instance CreateEntity AddResourcePageEvent ResourcePage where
  createEntity event =
    ResourcePage
      { uuid = event.entityUuid
      , title = event.title
      , content = event.content
      , annotations = event.annotations
      }

instance EditEntity EditResourcePageEvent ResourcePage where
  editEntity event entity =
    entity
      { title = applyValue entity.title event.title
      , content = applyValue entity.content event.content
      , annotations = applyValue entity.annotations event.annotations
      }
