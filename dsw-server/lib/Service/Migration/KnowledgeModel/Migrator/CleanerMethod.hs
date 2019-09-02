module Service.Migration.KnowledgeModel.Migrator.CleanerMethod where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)

import LensesConfig
import Model.Event.Event
import Model.Event.EventAccessors
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.Migration.KnowledgeModel.MigratorState

isCleanerMethod :: MigratorState -> Event -> Bool
isCleanerMethod state event = getKM $ \km -> doIsCleanerMethod km event
  where
    getKM callback =
      case state ^. currentKnowledgeModel of
        Just km -> callback km
        Nothing -> False

doIsCleanerMethod :: KnowledgeModel -> Event -> Bool
doIsCleanerMethod km (AddKnowledgeModelEvent' event) = False
doIsCleanerMethod km (EditKnowledgeModelEvent' event) = False
doIsCleanerMethod km (AddChapterEvent' event) = False
doIsCleanerMethod km (EditChapterEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. chaptersM)
doIsCleanerMethod km (DeleteChapterEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. chaptersM)
doIsCleanerMethod km (AddQuestionEvent' event) =
  isNothing (M.lookup (getEventParentUuid event) (km ^. chaptersM)) &&
  isNothing (M.lookup (getEventParentUuid event) (km ^. questionsM)) &&
  isNothing (M.lookup (getEventParentUuid event) (km ^. answersM))
doIsCleanerMethod km (EditQuestionEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. questionsM)
doIsCleanerMethod km (DeleteQuestionEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. questionsM)
doIsCleanerMethod km (AddAnswerEvent' event) = isNothing $ M.lookup (getEventParentUuid event) (km ^. questionsM)
doIsCleanerMethod km (EditAnswerEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. answersM)
doIsCleanerMethod km (DeleteAnswerEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. answersM)
doIsCleanerMethod km (AddExpertEvent' event) = isNothing $ M.lookup (getEventParentUuid event) (km ^. questionsM)
doIsCleanerMethod km (EditExpertEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. expertsM)
doIsCleanerMethod km (DeleteExpertEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. expertsM)
doIsCleanerMethod km (AddReferenceEvent' event) = isNothing $ M.lookup (getEventParentUuid event) (km ^. questionsM)
doIsCleanerMethod km (EditReferenceEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. referencesM)
doIsCleanerMethod km (DeleteReferenceEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. referencesM)
doIsCleanerMethod km (AddTagEvent' event) = False
doIsCleanerMethod km (EditTagEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. tagsM)
doIsCleanerMethod km (DeleteTagEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. tagsM)
doIsCleanerMethod km (AddIntegrationEvent' event) = False
doIsCleanerMethod km (EditIntegrationEvent' event) = isNothing $ M.lookup (getEventNodeUuid event) (km ^. integrationsM)
doIsCleanerMethod km (DeleteIntegrationEvent' event) =
  isNothing $ M.lookup (getEventNodeUuid event) (km ^. integrationsM)

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. targetPackageEvents
  in return $ state & targetPackageEvents .~ newTargetPackageEvents
