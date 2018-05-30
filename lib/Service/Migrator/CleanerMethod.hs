module Service.Migrator.CleanerMethod where

import Control.Lens

import LensesConfig
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.Migrator.MigratorState

isCleanerMethod :: MigratorState -> Event -> Bool
isCleanerMethod state event = getKM $ \km -> doIsCleanerMethod km event
  where
    getKM callback =
      case state ^. msCurrentKnowledgeModel of
        Just km -> callback km
        Nothing -> False

doIsCleanerMethod :: KnowledgeModel -> Event -> Bool
doIsCleanerMethod km (AddKnowledgeModelEvent' event) = False
doIsCleanerMethod km (EditKnowledgeModelEvent' event) = False
doIsCleanerMethod km (AddChapterEvent' event) = False
doIsCleanerMethod km (EditChapterEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. chapterUuid)
doIsCleanerMethod km (DeleteChapterEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. chapterUuid)
doIsCleanerMethod km (AddQuestionEvent' event) =
  if not . null $ event ^. path
    then not $ isThereAnyChapterWithGivenUuid km ((last (event ^. path)) ^. uuid)
    else False
doIsCleanerMethod km (EditQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (DeleteQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (AddAnswerEvent' event) =
  if not . null $ event ^. path
    then not $ isThereAnyQuestionWithGivenUuid km ((last (event ^. path)) ^. uuid)
    else False
doIsCleanerMethod km (EditAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (DeleteAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (AddExpertEvent' event) =
  if not . null $ event ^. path
    then not $ isThereAnyQuestionWithGivenUuid km ((last (event ^. path)) ^. uuid)
    else False
doIsCleanerMethod km (EditExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. expertUuid)
doIsCleanerMethod km (DeleteExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. expertUuid)
doIsCleanerMethod km (AddReferenceEvent' event) =
  if not . null $ event ^. path
    then not $ isThereAnyQuestionWithGivenUuid km ((last (event ^. path)) ^. uuid)
    else False
doIsCleanerMethod km (EditReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (event ^. referenceUuid)
doIsCleanerMethod km (DeleteReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (event ^. referenceUuid)

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. msTargetPackageEvents
  in return $ state & msTargetPackageEvents .~ newTargetPackageEvents
