module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Move where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.Move.MoveEvent
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Move ()

instance ApplyEvent MoveQuestionEvent where
  apply event = Right . moveUnderAnswer . moveUnderQuestion . moveUnderChapter
    where
      moveUnderChapter km = km & entities . chapters .~ M.map (editEntity event) (km ^. entities . chapters)
      moveUnderQuestion km = km & entities . questions .~ M.map (editEntity event) (km ^. entities . questions)
      moveUnderAnswer km = km & entities . answers .~ M.map (editEntity event) (km ^. entities . answers)

instance ApplyEvent MoveAnswerEvent where
  apply event km = Right $ km & entities . questions .~ M.map (editEntity event) (km ^. entities . questions)

instance ApplyEvent MoveChoiceEvent where
  apply event km = Right $ km & entities . questions .~ M.map (editEntity event) (km ^. entities . questions)

instance ApplyEvent MoveExpertEvent where
  apply event km = Right $ km & entities . questions .~ M.map (editEntity event) (km ^. entities . questions)

instance ApplyEvent MoveReferenceEvent where
  apply event km = Right $ km & entities . questions .~ M.map (editEntity event) (km ^. entities . questions)
