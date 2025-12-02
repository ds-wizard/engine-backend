module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Reference where

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddReferenceEvent where
  apply =
    applyCreateEventWithParent getReferencesM setReferencesM getQuestionsM setQuestionsM getReferenceUuids setReferenceUuids

instance ApplyEvent EditReferenceEvent where
  apply = applyEditEvent getReferencesM setReferencesM

instance ApplyEvent DeleteReferenceEvent where
  apply event content km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getReferenceUuids setReferenceUuids $ deleteReference km event.entityUuid
