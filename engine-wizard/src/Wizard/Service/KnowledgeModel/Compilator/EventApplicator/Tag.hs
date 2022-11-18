module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Tag where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Event.EventLenses
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddTagEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {tagUuids = km.tagUuids ++ [getEntityUuid event]} :: KnowledgeModel
      addEntity km = setTagsM km $ M.insert (getEntityUuid event) (createEntity event) km.entities.tags :: KnowledgeModel

instance ApplyEvent EditTagEvent where
  apply = applyEditEvent getTagsM setTagsM

instance ApplyEvent DeleteTagEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {tagUuids = L.delete (getEntityUuid event) km.tagUuids} :: KnowledgeModel
      deleteEntity km = setTagsM km $ M.delete (getEntityUuid event) (getTagsM km)
      deleteEntityChildrenReference km = setQuestionsM km $ M.map (deleteTagReference event) km.entities.questions
