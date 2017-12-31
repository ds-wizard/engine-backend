module Service.Migrator.CleanerMethod where

import Control.Lens

import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Event
import Model.Event.Expert.AddExpertEvent
import Model.Event.Expert.DeleteExpertEvent
import Model.Event.Expert.EditExpertEvent
import Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.DeleteQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Reference.AddReferenceEvent
import Model.Event.Reference.DeleteReferenceEvent
import Model.Event.Reference.EditReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
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
doIsCleanerMethod km (EditChapterEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. echChapterUuid)
doIsCleanerMethod km (DeleteChapterEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. dchChapterUuid)
doIsCleanerMethod km (AddQuestionEvent' event) = not $ isThereAnyChapterWithGivenUuid km (event ^. aqChapterUuid)
doIsCleanerMethod km (EditQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. eqQuestionUuid)
doIsCleanerMethod km (DeleteQuestionEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. dqQuestionUuid)
doIsCleanerMethod km (AddAnswerEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. aansQuestionUuid)
doIsCleanerMethod km (EditAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. eansAnswerUuid)
doIsCleanerMethod km (DeleteAnswerEvent' event) = not $ isThereAnyAnswerWithGivenUuid km (event ^. dansAnswerUuid)
doIsCleanerMethod km (AddExpertEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. aexpQuestionUuid)
doIsCleanerMethod km (EditExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. eexpExpertUuid)
doIsCleanerMethod km (DeleteExpertEvent' event) = not $ isThereAnyExpertWithGivenUuid km (event ^. dexpExpertUuid)
doIsCleanerMethod km (AddReferenceEvent' event) = not $ isThereAnyQuestionWithGivenUuid km (event ^. arefQuestionUuid)
doIsCleanerMethod km (EditReferenceEvent' event) = not $ isThereAnyReferenceWithGivenUuid km (event ^. erefChapterUuid)
doIsCleanerMethod km (DeleteReferenceEvent' event) =
  not $ isThereAnyReferenceWithGivenUuid km (event ^. drefReferenceUuid)
doIsCleanerMethod km (AddFollowUpQuestionEvent' event) =
  not $ isThereAnyAnswerWithGivenUuid km (event ^. afuqAnswerUuid)
doIsCleanerMethod km (EditFollowUpQuestionEvent' event) =
  not $ isThereAnyQuestionWithGivenUuid km (event ^. efuqQuestionUuid)
doIsCleanerMethod km (DeleteFollowUpQuestionEvent' event) =
  not $ isThereAnyQuestionWithGivenUuid km (event ^. dfuqQuestionUuid)

runCleanerMethod :: MigratorState -> Event -> IO MigratorState
runCleanerMethod state event =
  let (_:newTargetPackageEvents) = state ^. msTargetPackageEvents
  in return $ state & msTargetPackageEvents .~ newTargetPackageEvents
