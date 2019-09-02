module Service.KnowledgeModel.Compilator.EventApplicator.Answer where

import Prelude hiding (lookup)

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.EventAccessors
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Delete
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Question ()
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddAnswerEvent where
  apply = applyCreateEventWithParent (entities . answers) (entities . questions) answerUuids' "Answer" "Question"

instance ApplyEvent EditAnswerEvent where
  apply = applyEditEvent (entities . answers) "Answer"

instance ApplyEvent DeleteAnswerEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM answerUuids' $ deleteAnswer km (getEventNodeUuid event)
