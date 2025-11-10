module Wizard.Service.KnowledgeModel.Compiler.Modifier.Resource where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddResourceCollectionEvent ResourceCollection where
  createEntity event content =
    ResourceCollection
      { uuid = event.entityUuid
      , title = content.title
      , resourcePageUuids = []
      , annotations = content.annotations
      }

instance EditEntity EditResourceCollectionEvent ResourceCollection where
  editEntity event content entity =
    entity
      { title = applyValue entity.title content.title
      , resourcePageUuids = applyValue entity.resourcePageUuids content.resourcePageUuids
      , annotations = applyValue entity.annotations content.annotations
      }

-- --------------------------------------------
instance CreateEntity AddResourcePageEvent ResourcePage where
  createEntity event content =
    ResourcePage
      { uuid = event.entityUuid
      , title = content.title
      , content = content.content
      , annotations = content.annotations
      }

instance EditEntity EditResourcePageEvent ResourcePage where
  editEntity event content entity =
    entity
      { title = applyValue entity.title content.title
      , content = applyValue entity.content content.content
      , annotations = applyValue entity.annotations content.annotations
      }
