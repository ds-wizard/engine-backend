module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Expert where

import Control.Lens ((^.))
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddExpertEvent where
  apply = applyCreateEventWithParent (entities . experts) (entities . questions) expertUuids' "Expert" "Question"

instance ApplyEvent EditExpertEvent where
  apply = applyEditEvent (entities . experts) "Expert"

instance ApplyEvent DeleteExpertEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM expertUuids' $ deleteExpert km (event ^. entityUuid')
