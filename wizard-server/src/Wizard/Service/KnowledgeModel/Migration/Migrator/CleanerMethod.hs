module Wizard.Service.KnowledgeModel.Migration.Migrator.CleanerMethod where

import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.UUID as U

import Shared.Common.Util.Logger
import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

isCleanerMethod :: KnowledgeModelMigration -> KnowledgeModelEvent -> Bool
isCleanerMethod state event = getKM $ \km -> doIsCleanerMethod km event
  where
    getKM callback = maybe False callback state.currentKnowledgeModel

doIsCleanerMethod :: KnowledgeModel -> KnowledgeModelEvent -> Bool
doIsCleanerMethod km event =
  case event.content of
    AddKnowledgeModelEvent' content -> False
    EditKnowledgeModelEvent' content -> False
    AddChapterEvent' content -> False
    EditChapterEvent' content -> isNothing $ M.lookup event.entityUuid (getChaptersM km)
    DeleteChapterEvent' content -> isNothing $ M.lookup event.entityUuid (getChaptersM km)
    AddQuestionEvent' content ->
      isNothing (M.lookup event.parentUuid (getChaptersM km))
        && isNothing (M.lookup event.parentUuid (getQuestionsM km))
        && isNothing (M.lookup event.parentUuid (getAnswersM km))
    EditQuestionEvent' content -> isNothing $ M.lookup event.entityUuid (getQuestionsM km)
    DeleteQuestionEvent' content -> isNothing $ M.lookup event.entityUuid (getQuestionsM km)
    AddAnswerEvent' content -> isNothing $ M.lookup event.parentUuid (getQuestionsM km)
    EditAnswerEvent' content -> isNothing $ M.lookup event.entityUuid (getAnswersM km)
    DeleteAnswerEvent' content -> isNothing $ M.lookup event.entityUuid (getAnswersM km)
    AddChoiceEvent' content -> isNothing $ M.lookup event.parentUuid (getQuestionsM km)
    EditChoiceEvent' content -> isNothing $ M.lookup event.entityUuid (getChoicesM km)
    DeleteChoiceEvent' content -> isNothing $ M.lookup event.entityUuid (getChoicesM km)
    AddExpertEvent' content -> isNothing $ M.lookup event.parentUuid (getQuestionsM km)
    EditExpertEvent' content -> isNothing $ M.lookup event.entityUuid (getExpertsM km)
    DeleteExpertEvent' content -> isNothing $ M.lookup event.entityUuid (getExpertsM km)
    AddReferenceEvent' content -> isNothing $ M.lookup event.parentUuid (getQuestionsM km)
    EditReferenceEvent' content -> isNothing $ M.lookup event.entityUuid (getReferencesM km)
    DeleteReferenceEvent' content -> isNothing $ M.lookup event.entityUuid (getReferencesM km)
    AddTagEvent' content -> False
    EditTagEvent' content -> isNothing $ M.lookup event.entityUuid (getTagsM km)
    DeleteTagEvent' content -> isNothing $ M.lookup event.entityUuid (getTagsM km)
    AddIntegrationEvent' content -> False
    EditIntegrationEvent' content -> isNothing $ M.lookup event.entityUuid (getIntegrationsM km)
    DeleteIntegrationEvent' content -> isNothing $ M.lookup event.entityUuid (getIntegrationsM km)
    AddMetricEvent' content -> False
    EditMetricEvent' content -> isNothing $ M.lookup event.entityUuid (getMetricsM km)
    DeleteMetricEvent' content -> isNothing $ M.lookup event.entityUuid (getMetricsM km)
    AddPhaseEvent' content -> False
    EditPhaseEvent' content -> isNothing $ M.lookup event.entityUuid (getPhasesM km)
    DeletePhaseEvent' content -> isNothing $ M.lookup event.entityUuid (getPhasesM km)
    AddResourceCollectionEvent' content -> False
    EditResourceCollectionEvent' content -> isNothing $ M.lookup event.entityUuid (getResourceCollectionsM km)
    DeleteResourceCollectionEvent' content -> isNothing $ M.lookup event.entityUuid (getResourceCollectionsM km)
    AddResourcePageEvent' content -> isNothing $ M.lookup event.parentUuid (getResourceCollectionsM km)
    EditResourcePageEvent' content -> isNothing $ M.lookup event.entityUuid (getResourcePagesM km)
    DeleteResourcePageEvent' content -> isNothing $ M.lookup event.entityUuid (getResourcePagesM km)
    MoveQuestionEvent' content ->
      isNothing (M.lookup event.entityUuid (getQuestionsM km))
        || ( isNothing (M.lookup content.targetUuid (getChaptersM km))
              && isNothing (M.lookup content.targetUuid (getQuestionsM km))
              && isNothing (M.lookup content.targetUuid (getAnswersM km))
           )
    MoveAnswerEvent' content ->
      isNothing (M.lookup event.entityUuid (getAnswersM km))
        || isNothing (M.lookup content.targetUuid (getQuestionsM km))
    MoveChoiceEvent' content ->
      isNothing (M.lookup event.entityUuid (getChoicesM km))
        || isNothing (M.lookup content.targetUuid (getQuestionsM km))
    MoveExpertEvent' content ->
      isNothing (M.lookup event.entityUuid (getExpertsM km))
        || isNothing (M.lookup content.targetUuid (getQuestionsM km))
    MoveReferenceEvent' content ->
      isNothing (M.lookup event.entityUuid (getReferencesM km))
        || isNothing (M.lookup content.targetUuid (getQuestionsM km))

runCleanerMethod :: KnowledgeModelMigration -> KnowledgeModelEvent -> AppContextM KnowledgeModelMigration
runCleanerMethod state event = do
  logInfoI _CMP_SERVICE . f' "Running cleaner method for event '%s'" $ [U.toString event.uuid]
  let (_ : newTargetPackageEvents) = state.targetPackageEvents
  return $ state {targetPackageEvents = newTargetPackageEvents}
