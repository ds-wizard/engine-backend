module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Move where

import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Move ()

instance ApplyEvent MoveQuestionEvent where
  apply event content = Right . moveUnderAnswer . moveUnderQuestion . moveUnderChapter
    where
      moveUnderChapter km = setChaptersM km $ M.map (editEntity event content) km.entities.chapters
      moveUnderQuestion km = setQuestionsM km $ M.map (editEntity event content) km.entities.questions
      moveUnderAnswer km = setAnswersM km $ M.map (editEntity event content) km.entities.answers

instance ApplyEvent MoveAnswerEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions

instance ApplyEvent MoveChoiceEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions

instance ApplyEvent MoveExpertEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions

instance ApplyEvent MoveReferenceEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions
