module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Reference where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
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

instance ApplyEvent AddReferenceEvent where
  apply =
    applyCreateEventWithParent (entities . references) (entities . questions) referenceUuids' "Reference" "Question"

instance ApplyEvent EditReferenceEvent where
  apply = applyEditEvent (entities . references) "Reference"

instance ApplyEvent DeleteReferenceEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM referenceUuids' $ deleteReference km (event ^. entityUuid')
