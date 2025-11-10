module Wizard.Service.KnowledgeModel.Compiler.Compiler (
  compile,
) where

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelDM
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

compile :: Maybe KnowledgeModel -> [KnowledgeModelEvent] -> Either AppError KnowledgeModel
compile (Just km) events = foldl foldEvent (Right km) events
compile Nothing events = foldl foldEvent (Right defaultKnowledgeModel) events

-- --------------------------------
-- PRIVATE
-- --------------------------------
foldEvent :: Either AppError KnowledgeModel -> KnowledgeModelEvent -> Either AppError KnowledgeModel
foldEvent (Right km) event =
  case event.content of
    AddKnowledgeModelEvent' content -> apply event content km
    EditKnowledgeModelEvent' content -> apply event content km
    AddChapterEvent' content -> apply event content km
    EditChapterEvent' content -> apply event content km
    DeleteChapterEvent' content -> apply event content km
    AddQuestionEvent' content -> apply event content km
    EditQuestionEvent' content -> apply event content km
    DeleteQuestionEvent' content -> apply event content km
    AddAnswerEvent' content -> apply event content km
    EditAnswerEvent' content -> apply event content km
    DeleteAnswerEvent' content -> apply event content km
    AddChoiceEvent' content -> apply event content km
    EditChoiceEvent' content -> apply event content km
    DeleteChoiceEvent' content -> apply event content km
    AddExpertEvent' content -> apply event content km
    EditExpertEvent' content -> apply event content km
    DeleteExpertEvent' content -> apply event content km
    AddReferenceEvent' content -> apply event content km
    EditReferenceEvent' content -> apply event content km
    DeleteReferenceEvent' content -> apply event content km
    AddTagEvent' content -> apply event content km
    EditTagEvent' content -> apply event content km
    DeleteTagEvent' content -> apply event content km
    AddIntegrationEvent' content -> apply event content km
    EditIntegrationEvent' content -> apply event content km
    DeleteIntegrationEvent' content -> apply event content km
    AddMetricEvent' content -> apply event content km
    EditMetricEvent' content -> apply event content km
    DeleteMetricEvent' content -> apply event content km
    AddPhaseEvent' content -> apply event content km
    EditPhaseEvent' content -> apply event content km
    DeletePhaseEvent' content -> apply event content km
    AddResourceCollectionEvent' content -> apply event content km
    EditResourceCollectionEvent' content -> apply event content km
    DeleteResourceCollectionEvent' content -> apply event content km
    AddResourcePageEvent' content -> apply event content km
    EditResourcePageEvent' content -> apply event content km
    DeleteResourcePageEvent' content -> apply event content km
    MoveQuestionEvent' content -> apply event content km
    MoveAnswerEvent' content -> apply event content km
    MoveChoiceEvent' content -> apply event content km
    MoveExpertEvent' content -> apply event content km
    MoveReferenceEvent' content -> apply event content km
foldEvent (Left error) _ = Left error
