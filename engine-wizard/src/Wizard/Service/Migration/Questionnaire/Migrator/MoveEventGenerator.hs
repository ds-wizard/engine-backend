module Wizard.Service.Migration.Questionnaire.Migrator.MoveEventGenerator where

import Control.Lens ((^.))
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Shared.Model.Event.Event
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.KnowledgeModel.KnowledgeModelUtil

generateEvents :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [Event]
generateEvents now oldKm newKm = moveQuestionEvents ++ moveAnswerEvents
  where
    moveQuestionEvents = foldl (generateQuestionMoveEvent now oldParentMap newParentMap) [] (newKm ^. questionsL)
    moveAnswerEvents = foldl (generateAnswerMoveEvents now oldParentMap newParentMap) [] (newKm ^. answersL)
    oldParentMap = makeParentMap oldKm
    newParentMap = makeParentMap newKm

generateQuestionMoveEvent :: UTCTime -> KMParentMap -> KMParentMap -> [Event] -> Question -> [Event]
generateQuestionMoveEvent now oldParentMap newParentMap events entity =
  fromMaybe events $ do
    oldParentUuid <- M.lookup (entity ^. uuid') oldParentMap
    newParentUuid <- M.lookup (entity ^. uuid') newParentMap
    _ <- guard $ newParentUuid /= oldParentUuid
    let event =
          MoveQuestionEvent'
            MoveQuestionEvent
              { _moveQuestionEventUuid = U.nil
              , _moveQuestionEventParentUuid = oldParentUuid
              , _moveQuestionEventEntityUuid = entity ^. uuid'
              , _moveQuestionEventTargetUuid = newParentUuid
              , _moveQuestionEventCreatedAt = now
              }
    return $ events ++ [event]

generateAnswerMoveEvents :: UTCTime -> KMParentMap -> KMParentMap -> [Event] -> Answer -> [Event]
generateAnswerMoveEvents now oldParentMap newParentMap events entity =
  fromMaybe events $ do
    oldParentUuid <- M.lookup (entity ^. uuid') oldParentMap
    newParentUuid <- M.lookup (entity ^. uuid') newParentMap
    _ <- guard $ newParentUuid /= oldParentUuid
    let event =
          MoveAnswerEvent'
            MoveAnswerEvent
              { _moveAnswerEventUuid = U.nil
              , _moveAnswerEventParentUuid = oldParentUuid
              , _moveAnswerEventEntityUuid = entity ^. uuid'
              , _moveAnswerEventTargetUuid = newParentUuid
              , _moveAnswerEventCreatedAt = now
              }
    return $ events ++ [event]
