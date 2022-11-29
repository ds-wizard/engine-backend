module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Expert where

import Prelude hiding (lookup)

import Shared.Model.Event.EventLenses
import Shared.Model.Event.Expert.ExpertEvent
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddExpertEvent where
  apply = applyCreateEventWithParent getExpertsM setExpertsM getQuestionsM setQuestionsM getExpertUuids setExpertUuids

instance ApplyEvent EditExpertEvent where
  apply = applyEditEvent getExpertsM setExpertsM

instance ApplyEvent DeleteExpertEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getExpertUuids setExpertUuids $ deleteExpert km (getEntityUuid event)
