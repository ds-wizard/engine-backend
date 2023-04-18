module Wizard.Service.Migration.Questionnaire.Migrator.MoveEventGenerator where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelUtil

generateEvents :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [Event]
generateEvents now oldKm newKm = moveQuestionEvents ++ moveAnswerEvents
  where
    moveQuestionEvents = foldl (generateQuestionMoveEvent now oldParentMap newParentMap) [] (getQuestionsL newKm)
    moveAnswerEvents = foldl (generateAnswerMoveEvents now oldParentMap newParentMap) [] (getAnswersL newKm)
    oldParentMap = makeParentMap oldKm
    newParentMap = makeParentMap newKm

generateQuestionMoveEvent :: UTCTime -> KMParentMap -> KMParentMap -> [Event] -> Question -> [Event]
generateQuestionMoveEvent now oldParentMap newParentMap events entity =
  fromMaybe events $ do
    oldParentUuid <- M.lookup (getUuid entity) oldParentMap
    newParentUuid <- M.lookup (getUuid entity) newParentMap
    _ <- guard $ newParentUuid /= oldParentUuid
    let event =
          MoveQuestionEvent'
            MoveQuestionEvent
              { uuid = U.nil
              , parentUuid = oldParentUuid
              , entityUuid = getUuid entity
              , targetUuid = newParentUuid
              , createdAt = now
              }
    return $ events ++ [event]

generateAnswerMoveEvents :: UTCTime -> KMParentMap -> KMParentMap -> [Event] -> Answer -> [Event]
generateAnswerMoveEvents now oldParentMap newParentMap events entity =
  fromMaybe events $ do
    oldParentUuid <- M.lookup (getUuid entity) oldParentMap
    newParentUuid <- M.lookup (getUuid entity) newParentMap
    _ <- guard $ newParentUuid /= oldParentUuid
    let event =
          MoveAnswerEvent'
            MoveAnswerEvent
              { uuid = U.nil
              , parentUuid = oldParentUuid
              , entityUuid = getUuid entity
              , targetUuid = newParentUuid
              , createdAt = now
              }
    return $ events ++ [event]
