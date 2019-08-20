module Service.KnowledgeModel.Compilator.EventApplicator.Reference where

import LensesConfig
import Model.Event.EventAccessors
import Model.Event.Reference.ReferenceEvent
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

instance ApplyEvent AddReferenceEvent where
  apply =
    applyCreateEventWithParent (entities . references) (entities . questions) referenceUuids' "Reference" "Question"

instance ApplyEvent EditReferenceEvent where
  apply = applyEditEvent (entities . references) "Reference"

instance ApplyEvent DeleteReferenceEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM referenceUuids' $ deleteReference km (getEventNodeUuid event)
