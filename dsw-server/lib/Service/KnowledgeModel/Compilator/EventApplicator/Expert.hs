module Service.KnowledgeModel.Compilator.EventApplicator.Expert where

import Prelude hiding (lookup)

import LensesConfig
import Model.Event.EventAccessors
import Model.Event.Expert.ExpertEvent
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Delete
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddExpertEvent where
  apply = applyCreateEventWithParent (entities . experts) (entities . questions) expertUuids' "Expert" "Question"

instance ApplyEvent EditExpertEvent where
  apply = applyEditEvent (entities . experts) "Expert"

instance ApplyEvent DeleteExpertEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM expertUuids' $ deleteExpert km (getEventNodeUuid event)
