module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Choice where

import Control.Lens ((^.))
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.EventLenses
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Choice ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

instance ApplyEvent AddChoiceEvent where
  apply = applyCreateEventWithParent (entities . choices) (entities . questions) choiceUuids' "Choice" "Question"

instance ApplyEvent EditChoiceEvent where
  apply = applyEditEvent (entities . choices) "Choice"

instance ApplyEvent DeleteChoiceEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM choiceUuids' $ deleteChoice km (event ^. entityUuid')
