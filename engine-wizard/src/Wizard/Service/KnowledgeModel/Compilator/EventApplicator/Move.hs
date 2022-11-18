module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Move where

import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Move ()

instance ApplyEvent MoveQuestionEvent where
  apply event = Right . moveUnderAnswer . moveUnderQuestion . moveUnderChapter
    where
      moveUnderChapter km = setChaptersM km $ M.map (editEntity event) km.entities.chapters
      moveUnderQuestion km = setQuestionsM km $ M.map (editEntity event) km.entities.questions
      moveUnderAnswer km = setAnswersM km $ M.map (editEntity event) km.entities.answers

instance ApplyEvent MoveAnswerEvent where
  apply event km = Right . setQuestionsM km $ M.map (editEntity event) km.entities.questions

instance ApplyEvent MoveChoiceEvent where
  apply event km = Right . setQuestionsM km $ M.map (editEntity event) km.entities.questions

instance ApplyEvent MoveExpertEvent where
  apply event km = Right . setQuestionsM km $ M.map (editEntity event) km.entities.questions

instance ApplyEvent MoveReferenceEvent where
  apply event km = Right . setQuestionsM km $ M.map (editEntity event) km.entities.questions
