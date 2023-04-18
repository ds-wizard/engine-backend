module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Reference where

import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
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
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Reference.ReferenceEvent

instance ApplyEvent AddReferenceEvent where
  apply =
    applyCreateEventWithParent getReferencesM setReferencesM getQuestionsM setQuestionsM getReferenceUuids setReferenceUuids

instance ApplyEvent EditReferenceEvent where
  apply = applyEditEvent getReferencesM setReferencesM

instance ApplyEvent DeleteReferenceEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getReferenceUuids setReferenceUuids $ deleteReference km (getEntityUuid event)
