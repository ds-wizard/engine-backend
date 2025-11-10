module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Answer where

import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEventLenses ()
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

instance ApplyEvent AddAnswerEvent where
  apply = applyCreateEventWithParent getAnswersM setAnswersM getQuestionsM setQuestionsM getAnswerUuids setAnswerUuids

instance ApplyEvent EditAnswerEvent where
  apply = applyEditEvent getAnswersM setAnswersM

instance ApplyEvent DeleteAnswerEvent where
  apply event content km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getAnswerUuids setAnswerUuids $ deleteAnswer km event.entityUuid
