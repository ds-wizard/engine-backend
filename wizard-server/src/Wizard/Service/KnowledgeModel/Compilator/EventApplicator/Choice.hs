module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Choice where

import Prelude hiding (lookup)

import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Choice ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent
import WizardLib.KnowledgeModel.Model.Event.EventLenses

instance ApplyEvent AddChoiceEvent where
  apply = applyCreateEventWithParent getChoicesM setChoicesM getQuestionsM setQuestionsM getChoiceUuids setChoiceUuids

instance ApplyEvent EditChoiceEvent where
  apply = applyEditEvent getChoicesM setChoicesM

instance ApplyEvent DeleteChoiceEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getChoiceUuids setChoiceUuids $ deleteChoice km (getEntityUuid event)
