module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Answer where

import Control.Lens ((^.))
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.EventLenses
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddAnswerEvent where
  apply = applyCreateEventWithParent (entities . answers) (entities . questions) answerUuids' "Answer" "Question"

instance ApplyEvent EditAnswerEvent where
  apply = applyEditEvent (entities . answers) "Answer"

instance ApplyEvent DeleteAnswerEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM answerUuids' $ deleteAnswer km (event ^. entityUuid')
