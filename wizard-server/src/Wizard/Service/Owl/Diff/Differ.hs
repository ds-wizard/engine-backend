module Wizard.Service.Owl.Diff.Differ (
  diffKnowledgeModel,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (catMaybes)
import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.Answer ()
import Wizard.Service.Owl.Diff.EventFactory.Chapter ()
import Wizard.Service.Owl.Diff.EventFactory.Choice ()
import Wizard.Service.Owl.Diff.EventFactory.EventFactory
import Wizard.Service.Owl.Diff.EventFactory.Question ()

diffKnowledgeModel :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> m [KnowledgeModelEvent]
diffKnowledgeModel (oldKm, newKm) = do
  let oldChapters = getChaptersForKmUuid oldKm
  let newChapters = getChaptersForKmUuid newKm
  existingChaptersDiff <- traverse (diffChapter (oldKm, newKm)) (getExistingEntities oldChapters newChapters)
  newChapterEvents <- traverse (createAddEvent newKm.uuid) (getDiffEntities oldChapters newChapters)
  deleteChapterEvents <- traverse (createDeleteEvent newKm.uuid) (getDiffEntities newChapters oldChapters)
  return $ concat existingChaptersDiff ++ newChapterEvents ++ deleteChapterEvents

diffChapter :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> (Chapter, Chapter) -> m [KnowledgeModelEvent]
diffChapter (oldKm, newKm) (oldCh, newCh) = do
  editChapterEvent <- createEditEvent (oldKm, newKm) U.nil oldCh newCh
  let oldQuestions = getQuestionsForChapterUuid oldKm oldCh.uuid
  let newQuestions = getQuestionsForChapterUuid newKm newCh.uuid
  existingQuestionsDiff <- traverse (diffQuestion (oldKm, newKm)) (getExistingEntities oldQuestions newQuestions)
  newQuestionEvents <- traverse (createAddEvent newCh.uuid) (getDiffEntities oldQuestions newQuestions)
  deletedQuestionEvents <- traverse (createDeleteEvent newCh.uuid) (getDiffEntities newQuestions oldQuestions)
  return $ catMaybes [editChapterEvent] ++ concat existingQuestionsDiff ++ newQuestionEvents ++ deletedQuestionEvents

diffQuestion :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> (Question, Question) -> m [KnowledgeModelEvent]
diffQuestion (oldKm, newKm) (OptionsQuestion' oldQ, OptionsQuestion' newQ) = do
  editQuestionEvent <- createEditEvent (oldKm, newKm) U.nil (OptionsQuestion' oldQ) (OptionsQuestion' newQ)
  let oldAnswers = getAnswersForQuestionUuid oldKm oldQ.uuid
  let newAnswers = getAnswersForQuestionUuid newKm newQ.uuid
  existingAnswersDiff <- traverse (diffAnswer (oldKm, newKm)) (getExistingEntities oldAnswers newAnswers)
  newAnswerEvents <- traverse (createAddEvent newQ.uuid) (getDiffEntities oldAnswers newAnswers)
  deletedAnswerEvents <- traverse (createDeleteEvent oldQ.uuid) (getDiffEntities newAnswers oldAnswers)
  return $ catMaybes [editQuestionEvent] ++ concat existingAnswersDiff ++ newAnswerEvents ++ deletedAnswerEvents
diffQuestion (oldKm, newKm) (MultiChoiceQuestion' oldQ, MultiChoiceQuestion' newQ) = do
  editQuestionEvent <- createEditEvent (oldKm, newKm) U.nil (MultiChoiceQuestion' oldQ) (MultiChoiceQuestion' newQ)
  let oldChoices = getChoicesForQuestionUuid oldKm oldQ.uuid
  let newChoices = getChoicesForQuestionUuid newKm newQ.uuid
  existingChoicesDiff <- traverse (diffChoice (oldKm, newKm)) (getExistingEntities oldChoices newChoices)
  newChoiceEvents <- traverse (createAddEvent newQ.uuid) (getDiffEntities oldChoices newChoices)
  deletedChoiceEvents <- traverse (createDeleteEvent oldQ.uuid) (getDiffEntities newChoices oldChoices)
  return $ catMaybes [editQuestionEvent] ++ concat existingChoicesDiff ++ newChoiceEvents ++ deletedChoiceEvents
diffQuestion (oldKm, newKm) (ListQuestion' oldQ, ListQuestion' newQ) = do
  editQuestionEvent <- createEditEvent (oldKm, newKm) U.nil (ListQuestion' oldQ) (ListQuestion' newQ)
  let oldQuestions = getItemTemplateQuestionsForQuestionUuid oldKm oldQ.uuid
  let newQuestions = getItemTemplateQuestionsForQuestionUuid newKm newQ.uuid
  existingQuestionsDiff <- traverse (diffQuestion (oldKm, newKm)) (getExistingEntities oldQuestions newQuestions)
  newQuestionEvents <- traverse (createAddEvent newQ.uuid) (getDiffEntities oldQuestions newQuestions)
  deletedQuestionEvents <- traverse (createDeleteEvent oldQ.uuid) (getDiffEntities newQuestions oldQuestions)
  return $ catMaybes [editQuestionEvent] ++ concat existingQuestionsDiff ++ newQuestionEvents ++ deletedQuestionEvents
diffQuestion (oldKm, newKm) (ValueQuestion' oldQ, ValueQuestion' newQ) = do
  editQuestionEvent <- createEditEvent (oldKm, newKm) U.nil (ValueQuestion' oldQ) (ValueQuestion' newQ)
  return $ catMaybes [editQuestionEvent]
diffQuestion (oldKm, newKm) _ = return []

diffAnswer :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> (Answer, Answer) -> m [KnowledgeModelEvent]
diffAnswer (oldKm, newKm) (oldAnswer, newAnswer) = do
  editAnswerEvent <- createEditEvent (oldKm, newKm) U.nil oldAnswer newAnswer
  let oldQuestions = getQuestionsForAnswerUuid oldKm oldAnswer.uuid
  let newQuestions = getQuestionsForAnswerUuid newKm newAnswer.uuid
  existingQuestionsDiff <- traverse (diffQuestion (oldKm, newKm)) (getExistingEntities oldQuestions newQuestions)
  newQuestionEvents <- traverse (createAddEvent newAnswer.uuid) (getDiffEntities oldQuestions newQuestions)
  deletedQuestionEvents <- traverse (createDeleteEvent oldAnswer.uuid) (getDiffEntities newQuestions oldQuestions)
  return $ catMaybes [editAnswerEvent] ++ concat existingQuestionsDiff ++ newQuestionEvents ++ deletedQuestionEvents

diffChoice :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> (Choice, Choice) -> m [KnowledgeModelEvent]
diffChoice (oldKm, newKm) (oldChoice, newChoice) = do
  editChoiceEvent <- createEditEvent (oldKm, newKm) U.nil oldChoice newChoice
  return $ catMaybes [editChoiceEvent]
