module Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizator where

import Control.Lens ((^.))
import Control.Monad (guard)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Prelude hiding (id)
import qualified Prelude

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.Questionnaire.QuestionnaireUtil
import Shared.Util.List (tailSafe)
import Shared.Util.String (replace)
import Wizard.Model.Questionnaire.QuestionnaireReply

sanitizeReplies :: KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies oldKm newKm replies = sanitizeRepliesWithEvents newKm replies (generateEvents oldKm newKm)

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeRepliesWithEvents :: KnowledgeModel -> [ReplyTuple] -> [Event] -> [ReplyTuple]
sanitizeRepliesWithEvents km = foldl (processReplies km)

generateEvents :: KnowledgeModel -> KnowledgeModel -> [Event]
generateEvents oldKm newKm = moveQuestionEvents ++ moveAnswerEvents
  where
    moveQuestionEvents = foldl (generateQuestionMoveEvent oldParentMap newParentMap) [] (newKm ^. questionsL)
    moveAnswerEvents = foldl (generateAnswerMoveEvents oldParentMap newParentMap) [] (newKm ^. answersL)
    oldParentMap = makeParentMap oldKm
    newParentMap = makeParentMap newKm

generateQuestionMoveEvent :: KMParentMap -> KMParentMap -> [Event] -> Question -> [Event]
generateQuestionMoveEvent oldParentMap newParentMap events entity =
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
              }
    return $ events ++ [event]

generateAnswerMoveEvents :: KMParentMap -> KMParentMap -> [Event] -> Answer -> [Event]
generateAnswerMoveEvents oldParentMap newParentMap events entity =
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
              }
    return $ events ++ [event]

-- ----------------------------------------------------------------------
-- ----------------------------------------------------------------------
processReplies :: KnowledgeModel -> [ReplyTuple] -> Event -> [ReplyTuple]
processReplies km replies (MoveQuestionEvent' event) = processRepliesForQuestionMove km event replies
processReplies km replies (MoveAnswerEvent' event) = deleteUnwantedReplies (event ^. entityUuid) replies
processReplies _ replies _ = replies

processRepliesForQuestionMove ::
     (HasParentUuid event U.UUID, HasTargetUuid event U.UUID, HasEntityUuid event U.UUID)
  => KnowledgeModel
  -> event
  -> [ReplyTuple]
  -> [ReplyTuple]
processRepliesForQuestionMove km event replies =
  let parentMap = makeParentMap km
      pPath = computeParentPath parentMap (event ^. parentUuid)
      tPath = computeParentPath parentMap (event ^. targetUuid)
      sharedNode = computeSharedNode pPath tPath
      pPathDiff = takeDiffSuffix sharedNode pPath
      tPathDiff = takeDiffSuffix sharedNode tPath
   in doMigration km (event ^. entityUuid) pPathDiff tPathDiff replies

computeParentPath :: KMParentMap -> U.UUID -> [U.UUID]
computeParentPath parentMap eUuid =
  case M.lookup eUuid parentMap of
    Just parentUuid -> computeParentPath parentMap parentUuid ++ [eUuid]
    Nothing -> [eUuid]

computeSharedNode :: [U.UUID] -> [U.UUID] -> Maybe U.UUID
computeSharedNode pPath tPath = foldl go Nothing (reverse pPath)
  where
    go Nothing nUuid =
      if nUuid `elem` tPath
        then Just nUuid
        else Nothing
    go result _ = result

takeSharedPrefix :: Maybe U.UUID -> [U.UUID] -> [U.UUID]
takeSharedPrefix (Just sharedNode) = takeWhile (/= sharedNode)
takeSharedPrefix Nothing = Prelude.id

takeDiffSuffix :: Maybe U.UUID -> [U.UUID] -> [U.UUID]
takeDiffSuffix (Just sharedNode) = tailSafe . dropWhile (/= sharedNode)
takeDiffSuffix Nothing = Prelude.id

shouldWeMigrate :: KnowledgeModel -> [U.UUID] -> [U.UUID] -> Bool
shouldWeMigrate km pPathDiff tPathDiff = not . or . fmap (isItListQuestion km) $ pPathDiff ++ tPathDiff

doMigration :: KnowledgeModel -> U.UUID -> [U.UUID] -> [U.UUID] -> [ReplyTuple] -> [ReplyTuple]
doMigration km eUuid pPathDiff tPathDiff replies =
  if shouldWeMigrate km pPathDiff tPathDiff
    then computeDesiredPath eUuid pPathDiff tPathDiff replies
    else deleteUnwantedReplies eUuid replies

computeDesiredPath :: U.UUID -> [U.UUID] -> [U.UUID] -> [ReplyTuple] -> [ReplyTuple]
computeDesiredPath eUuid pPathDiff tPathDiff = fmap (replaceReply pPathDiffS tPathDiffS)
  where
    replaceReply pPathDiffS tPathDiffS reply@(path, _) =
      if path `replyKeyContains` eUuid
        then replaceReply' pPathDiffS tPathDiffS reply
        else reply
    replaceReply' "" _ (path, value) = (path ++ "." ++ tPathDiffS, value)
    replaceReply' pPathDiffS tPathDiffS (path, value) = (replace pPathDiffS tPathDiffS path, value)
    pPathDiffS = createReplyKey pPathDiff
    tPathDiffS = createReplyKey tPathDiff

deleteUnwantedReplies :: U.UUID -> [ReplyTuple] -> [ReplyTuple]
deleteUnwantedReplies eUuid = filter (\(path, _) -> not $ U.toString eUuid `L.isInfixOf` path)
