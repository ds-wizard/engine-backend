module Service.Migration.KnowledgeModel.CleanerMethod where

import Control.Lens

import LensesConfig
import Model.Event.Event
import Model.Event.EventAccessors
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
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
doIsCleanerMethod km (EditChapterEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. chapterUuid)
doIsCleanerMethod km (DeleteChapterEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. chapterUuid)
doIsCleanerMethod km (AddQuestionEvent' event) =
  if not . null $ getPath event
    then not $ isThereAnyChapterWithGivenUuid km ((last (getPath event)) ^. uuid)
    else False
doIsCleanerMethod km (EditQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (getEventQuestionUuid event)
doIsCleanerMethod km (DeleteQuestionEvent' event) =
  not $ isThereAnyQuestionWithGivenUuid km (getEventQuestionUuid event)
doIsCleanerMethod km (AddAnswerEvent' event) =
  if not . null $ getPath event
    then not $ isThereAnyQuestionWithGivenUuid km ((last (getPath event)) ^. uuid)
    else False
doIsCleanerMethod km (EditAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (DeleteAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (AddExpertEvent' event) =
  if not . null $ getPath event
    then not $ isThereAnyQuestionWithGivenUuid km ((last (getPath event)) ^. uuid)
    else False
doIsCleanerMethod km (EditExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. expertUuid)
doIsCleanerMethod km (DeleteExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. expertUuid)
doIsCleanerMethod km (AddReferenceEvent' event) =
  if not . null . getPath $ event
    then not $ isThereAnyQuestionWithGivenUuid km ((last (getPath event)) ^. uuid)
    else False
doIsCleanerMethod km (EditReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (getEventNodeUuid event)
doIsCleanerMethod km (DeleteReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (getEventNodeUuid event)
doIsCleanerMethod km (AddTagEvent' event) = False
doIsCleanerMethod km (EditTagEvent' event) = not $ isThereAnyTagWithGivenUuid km (event ^. tagUuid)
doIsCleanerMethod km (DeleteTagEvent' event) = not $ isThereAnyTagWithGivenUuid km (event ^. tagUuid)
doIsCleanerMethod km (AddIntegrationEvent' event) = False
doIsCleanerMethod km (EditIntegrationEvent' event) =
  not $ isThereAnyIntegrationWithGivenUuid km (event ^. integrationUuid)
doIsCleanerMethod km (DeleteIntegrationEvent' event) =
  not $ isThereAnyIntegrationWithGivenUuid km (event ^. integrationUuid)

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. targetPackageEvents
  in return $ state & targetPackageEvents .~ newTargetPackageEvents
