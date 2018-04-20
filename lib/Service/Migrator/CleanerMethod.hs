module Service.Migrator.CleanerMethod where

import Control.Lens

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
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
doIsCleanerMethod km (AddQuestionEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. chapterUuid)
doIsCleanerMethod km (EditQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (DeleteQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (AddAnswerEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (EditAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (DeleteAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (AddExpertEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (EditExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. expertUuid)
doIsCleanerMethod km (DeleteExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. expertUuid)
doIsCleanerMethod km (AddReferenceEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (EditReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (event ^. chapterUuid)
doIsCleanerMethod km (DeleteReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (event ^. referenceUuid)
doIsCleanerMethod km (AddFollowUpQuestionEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. answerUuid)
doIsCleanerMethod km (EditFollowUpQuestionEvent' event) =
  not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)
doIsCleanerMethod km (DeleteFollowUpQuestionEvent' event) =
  not $ isThereAnyQuestionWithGivenUuid km (event ^. questionUuid)

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. msTargetPackageEvents
  in return $ state & msTargetPackageEvents .~ newTargetPackageEvents
