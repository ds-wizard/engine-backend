module Wizard.Service.Migration.KnowledgeModel.Migrator.CleanerMethod where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.Event.EventAccessors
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Model.Migration.KnowledgeModel.MigratorState

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
doIsCleanerMethod km (MoveQuestionEvent' event) =
  isNothing (M.lookup (getEventNodeUuid event) (km ^. questionsM)) ||
  (isNothing (M.lookup (event ^. targetUuid) (km ^. chaptersM)) &&
   isNothing (M.lookup (event ^. targetUuid) (km ^. questionsM)) &&
   isNothing (M.lookup (event ^. targetUuid) (km ^. answersM)))
doIsCleanerMethod km (MoveAnswerEvent' event) =
  isNothing (M.lookup (getEventNodeUuid event) (km ^. answersM)) ||
  isNothing (M.lookup (event ^. targetUuid) (km ^. questionsM))
doIsCleanerMethod km (MoveExpertEvent' event) =
  isNothing (M.lookup (getEventNodeUuid event) (km ^. expertsM)) ||
  isNothing (M.lookup (event ^. targetUuid) (km ^. questionsM))
doIsCleanerMethod km (MoveReferenceEvent' event) =
  isNothing (M.lookup (getEventNodeUuid event) (km ^. referencesM)) ||
  isNothing (M.lookup (event ^. targetUuid) (km ^. questionsM))

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. targetPackageEvents
   in return $ state & targetPackageEvents .~ newTargetPackageEvents
