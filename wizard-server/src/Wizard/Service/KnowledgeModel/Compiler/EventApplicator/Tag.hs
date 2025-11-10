module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Tag where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddTagEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {tagUuids = km.tagUuids ++ [event.entityUuid]} :: KnowledgeModel
      addEntity km = setTagsM km $ M.insert event.entityUuid (createEntity event content) km.entities.tags :: KnowledgeModel

instance ApplyEvent EditTagEvent where
  apply = applyEditEvent getTagsM setTagsM

instance ApplyEvent DeleteTagEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {tagUuids = L.delete event.entityUuid km.tagUuids} :: KnowledgeModel
      deleteEntity km = setTagsM km $ M.delete event.entityUuid (getTagsM km)
      deleteEntityChildrenReference km = setQuestionsM km $ M.map (deleteTagReference event) km.entities.questions
