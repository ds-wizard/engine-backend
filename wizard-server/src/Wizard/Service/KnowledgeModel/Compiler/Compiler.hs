module Wizard.Service.KnowledgeModel.Compiler.Compiler (
  compile,
) where

import Shared.Common.Model.Error.Error
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Choice ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Move ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Question ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Resource ()
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Tag ()
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelDM

compile :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
compile (Just km) events = foldl foldEvent (Right km) events
compile Nothing events = foldl foldEvent (Right defaultKnowledgeModel) events

-- --------------------------------
-- PRIVATE
-- --------------------------------
foldEvent :: Either AppError KnowledgeModel -> Event -> Either AppError KnowledgeModel
foldEvent (Right km) (AddKnowledgeModelEvent' e) = apply e km
foldEvent (Right km) (EditKnowledgeModelEvent' e) = apply e km
foldEvent (Right km) (AddChapterEvent' e) = apply e km
foldEvent (Right km) (EditChapterEvent' e) = apply e km
foldEvent (Right km) (DeleteChapterEvent' e) = apply e km
foldEvent (Right km) (AddQuestionEvent' e) = apply e km
foldEvent (Right km) (EditQuestionEvent' e) = apply e km
foldEvent (Right km) (DeleteQuestionEvent' e) = apply e km
foldEvent (Right km) (AddAnswerEvent' e) = apply e km
foldEvent (Right km) (EditAnswerEvent' e) = apply e km
foldEvent (Right km) (DeleteAnswerEvent' e) = apply e km
foldEvent (Right km) (AddChoiceEvent' e) = apply e km
foldEvent (Right km) (EditChoiceEvent' e) = apply e km
foldEvent (Right km) (DeleteChoiceEvent' e) = apply e km
foldEvent (Right km) (AddExpertEvent' e) = apply e km
foldEvent (Right km) (EditExpertEvent' e) = apply e km
foldEvent (Right km) (DeleteExpertEvent' e) = apply e km
foldEvent (Right km) (AddReferenceEvent' e) = apply e km
foldEvent (Right km) (EditReferenceEvent' e) = apply e km
foldEvent (Right km) (DeleteReferenceEvent' e) = apply e km
foldEvent (Right km) (AddTagEvent' e) = apply e km
foldEvent (Right km) (EditTagEvent' e) = apply e km
foldEvent (Right km) (DeleteTagEvent' e) = apply e km
foldEvent (Right km) (AddIntegrationEvent' e) = apply e km
foldEvent (Right km) (EditIntegrationEvent' e) = apply e km
foldEvent (Right km) (DeleteIntegrationEvent' e) = apply e km
foldEvent (Right km) (AddMetricEvent' e) = apply e km
foldEvent (Right km) (EditMetricEvent' e) = apply e km
foldEvent (Right km) (DeleteMetricEvent' e) = apply e km
foldEvent (Right km) (AddPhaseEvent' e) = apply e km
foldEvent (Right km) (EditPhaseEvent' e) = apply e km
foldEvent (Right km) (DeletePhaseEvent' e) = apply e km
foldEvent (Right km) (AddResourceCollectionEvent' e) = apply e km
foldEvent (Right km) (EditResourceCollectionEvent' e) = apply e km
foldEvent (Right km) (DeleteResourceCollectionEvent' e) = apply e km
foldEvent (Right km) (AddResourcePageEvent' e) = apply e km
foldEvent (Right km) (EditResourcePageEvent' e) = apply e km
foldEvent (Right km) (DeleteResourcePageEvent' e) = apply e km
foldEvent (Right km) (MoveQuestionEvent' e) = apply e km
foldEvent (Right km) (MoveAnswerEvent' e) = apply e km
foldEvent (Right km) (MoveChoiceEvent' e) = apply e km
foldEvent (Right km) (MoveExpertEvent' e) = apply e km
foldEvent (Right km) (MoveReferenceEvent' e) = apply e km
foldEvent (Left error) _ = Left error
