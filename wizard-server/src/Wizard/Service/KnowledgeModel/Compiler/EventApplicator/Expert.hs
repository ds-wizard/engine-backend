module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Expert where

import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddExpertEvent where
  apply = applyCreateEventWithParent getExpertsM setExpertsM getQuestionsM setQuestionsM getExpertUuids setExpertUuids

instance ApplyEvent EditExpertEvent where
  apply = applyEditEvent getExpertsM setExpertsM

instance ApplyEvent DeleteExpertEvent where
  apply event content km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getExpertUuids setExpertUuids $ deleteExpert km event.entityUuid
