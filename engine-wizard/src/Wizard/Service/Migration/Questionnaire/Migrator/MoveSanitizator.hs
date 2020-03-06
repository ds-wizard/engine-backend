module Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizator where

import Control.Lens ((&), (.~), (^.))
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
import Shared.Util.String (replace)
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Util.List (tailSafe)

sanitizeReplies :: KnowledgeModel -> KnowledgeModel -> [Reply] -> [Reply]
sanitizeReplies oldKm newKm replies = sanitizeRepliesWithEvents newKm replies (generateEvents oldKm newKm)

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeRepliesWithEvents :: KnowledgeModel -> [Reply] -> [Event] -> [Reply]
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
processReplies :: KnowledgeModel -> [Reply] -> Event -> [Reply]
processReplies km replies (MoveQuestionEvent' event) = processRepliesForQuestionMove km event replies
processReplies km replies (MoveAnswerEvent' event) = deleteUnwantedReplies (event ^. entityUuid) replies
processReplies _ replies _ = replies

processRepliesForQuestionMove ::
     (HasParentUuid event U.UUID, HasTargetUuid event U.UUID, HasEntityUuid event U.UUID)
  => KnowledgeModel
  -> event
  -> [Reply]
  -> [Reply]
processRepliesForQuestionMove km event replies =
  let parentMap = makeParentMap km
      pPath = computeParentPath parentMap (event ^. parentUuid)
      tPath = computeParentPath parentMap (event ^. targetUuid)
      sharedNode = computeSharedNode pPath tPath
      sharedPath = takeSharedPrefix sharedNode tPath
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

doMigration :: KnowledgeModel -> U.UUID -> [U.UUID] -> [U.UUID] -> [Reply] -> [Reply]
doMigration km eUuid pPathDiff tPathDiff replies =
  if shouldWeMigrate km pPathDiff tPathDiff
    then computeDesiredPath pPathDiff tPathDiff replies
    else deleteUnwantedReplies eUuid replies

computeDesiredPath :: [U.UUID] -> [U.UUID] -> [Reply] -> [Reply]
computeDesiredPath pPathDiff tPathDiff = fmap (replaceReply pPathDiffS tPathDiffS)
  where
    replaceReply "" _ r = r & path .~ ((r ^. path) ++ "." ++ tPathDiffS)
    replaceReply pPathDiffS tPathDiffS r = r & path .~ replace pPathDiffS tPathDiffS (r ^. path)
    pPathDiffS = createReplyKey . fmap U.toString $ pPathDiff
    tPathDiffS = createReplyKey . fmap U.toString $ tPathDiff

deleteUnwantedReplies :: U.UUID -> [Reply] -> [Reply]
deleteUnwantedReplies eUuid = filter (\r -> not $ U.toString eUuid `L.isInfixOf` (r ^. path))
