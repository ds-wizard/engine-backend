module Wizard.Service.KnowledgeModel.Compilator.Compilator
  ( compile
  ) where

import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelDM
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Choice ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Move ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Question ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Tag ()

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
foldEvent (Right km) (MoveQuestionEvent' e) = apply e km
foldEvent (Right km) (MoveAnswerEvent' e) = apply e km
foldEvent (Right km) (MoveChoiceEvent' e) = apply e km
foldEvent (Right km) (MoveExpertEvent' e) = apply e km
foldEvent (Right km) (MoveReferenceEvent' e) = apply e km
foldEvent (Left error) _ = Left error
