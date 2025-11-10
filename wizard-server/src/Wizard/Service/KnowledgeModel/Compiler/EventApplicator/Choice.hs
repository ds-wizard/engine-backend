module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Choice where

import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Choice ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddChoiceEvent where
  apply = applyCreateEventWithParent getChoicesM setChoicesM getQuestionsM setQuestionsM getChoiceUuids setChoiceUuids

instance ApplyEvent EditChoiceEvent where
  apply = applyEditEvent getChoicesM setChoicesM

instance ApplyEvent DeleteChoiceEvent where
  apply event content km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getChoiceUuids setChoiceUuids $ deleteChoice km event.entityUuid
