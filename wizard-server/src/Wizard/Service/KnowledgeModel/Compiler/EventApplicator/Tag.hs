module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Tag where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

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
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
