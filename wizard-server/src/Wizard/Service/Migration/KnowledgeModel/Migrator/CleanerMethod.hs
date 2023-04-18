module Wizard.Service.Migration.KnowledgeModel.Migrator.CleanerMethod where

import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)

import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses ()

isCleanerMethod :: MigratorState -> Event -> Bool
isCleanerMethod state event = getKM $ \km -> doIsCleanerMethod km event
  where
    getKM callback = maybe False callback state.currentKnowledgeModel

doIsCleanerMethod :: KnowledgeModel -> Event -> Bool
doIsCleanerMethod km (AddKnowledgeModelEvent' event) = False
doIsCleanerMethod km (EditKnowledgeModelEvent' event) = False
doIsCleanerMethod km (AddChapterEvent' event) = False
doIsCleanerMethod km (EditChapterEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getChaptersM km)
doIsCleanerMethod km (DeleteChapterEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getChaptersM km)
doIsCleanerMethod km (AddQuestionEvent' event) =
  isNothing (M.lookup (getParentUuid event) (getChaptersM km))
    && isNothing (M.lookup (getParentUuid event) (getQuestionsM km))
    && isNothing (M.lookup (getParentUuid event) (getAnswersM km))
doIsCleanerMethod km (EditQuestionEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getQuestionsM km)
doIsCleanerMethod km (DeleteQuestionEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getQuestionsM km)
doIsCleanerMethod km (AddAnswerEvent' event) = isNothing $ M.lookup (getParentUuid event) (getQuestionsM km)
doIsCleanerMethod km (EditAnswerEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getAnswersM km)
doIsCleanerMethod km (DeleteAnswerEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getAnswersM km)
doIsCleanerMethod km (AddChoiceEvent' event) = isNothing $ M.lookup (getParentUuid event) (getQuestionsM km)
doIsCleanerMethod km (EditChoiceEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getChoicesM km)
doIsCleanerMethod km (DeleteChoiceEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getChoicesM km)
doIsCleanerMethod km (AddExpertEvent' event) = isNothing $ M.lookup (getParentUuid event) (getQuestionsM km)
doIsCleanerMethod km (EditExpertEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getExpertsM km)
doIsCleanerMethod km (DeleteExpertEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getExpertsM km)
doIsCleanerMethod km (AddReferenceEvent' event) = isNothing $ M.lookup (getParentUuid event) (getQuestionsM km)
doIsCleanerMethod km (EditReferenceEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getReferencesM km)
doIsCleanerMethod km (DeleteReferenceEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getReferencesM km)
doIsCleanerMethod km (AddTagEvent' event) = False
doIsCleanerMethod km (EditTagEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getTagsM km)
doIsCleanerMethod km (DeleteTagEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getTagsM km)
doIsCleanerMethod km (AddIntegrationEvent' event) = False
doIsCleanerMethod km (EditIntegrationEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getIntegrationsM km)
doIsCleanerMethod km (DeleteIntegrationEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getIntegrationsM km)
doIsCleanerMethod km (AddMetricEvent' event) = False
doIsCleanerMethod km (EditMetricEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getMetricsM km)
doIsCleanerMethod km (DeleteMetricEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getMetricsM km)
doIsCleanerMethod km (AddPhaseEvent' event) = False
doIsCleanerMethod km (EditPhaseEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getPhasesM km)
doIsCleanerMethod km (DeletePhaseEvent' event) = isNothing $ M.lookup (getEntityUuid event) (getPhasesM km)
doIsCleanerMethod km (MoveQuestionEvent' event) =
  isNothing (M.lookup (getEntityUuid event) (getQuestionsM km))
    || ( isNothing (M.lookup event.targetUuid (getChaptersM km))
          && isNothing (M.lookup event.targetUuid (getQuestionsM km))
          && isNothing (M.lookup event.targetUuid (getAnswersM km))
       )
doIsCleanerMethod km (MoveAnswerEvent' event) =
  isNothing (M.lookup (getEntityUuid event) (getAnswersM km))
    || isNothing (M.lookup event.targetUuid (getQuestionsM km))
doIsCleanerMethod km (MoveChoiceEvent' event) =
  isNothing (M.lookup (getEntityUuid event) (getChoicesM km))
    || isNothing (M.lookup event.targetUuid (getQuestionsM km))
doIsCleanerMethod km (MoveExpertEvent' event) =
  isNothing (M.lookup (getEntityUuid event) (getExpertsM km))
    || isNothing (M.lookup event.targetUuid (getQuestionsM km))
doIsCleanerMethod km (MoveReferenceEvent' event) =
  isNothing (M.lookup (getEntityUuid event) (getReferencesM km))
    || isNothing (M.lookup event.targetUuid (getQuestionsM km))

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_ : newTargetPackageEvents) = state.targetPackageEvents
   in return $ state {targetPackageEvents = newTargetPackageEvents}
