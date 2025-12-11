module Wizard.Service.Project.Migration.Migrator.MoveSanitizer where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)
import qualified Prelude

import Shared.Common.Util.List (tailSafe)
import Shared.Common.Util.String (f', replace)
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelUtil
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.ProjectUtil
import Wizard.Service.Project.Migration.Migrator.MoveEventGenerator

sanitizeReplies :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies now oldKm newKm replies = sanitizeRepliesWithEvents oldKm newKm replies (generateEvents now oldKm newKm)

sanitizeRepliesWithEvents :: KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [KnowledgeModelEvent] -> [ReplyTuple]
sanitizeRepliesWithEvents oldKm newKm = foldl (processReplies oldKm newKm)

-- --------------------------------
-- PRIVATE
-- --------------------------------
processReplies :: KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> KnowledgeModelEvent -> [ReplyTuple]
processReplies oldKm newKm replies event =
  case event.content of
    MoveQuestionEvent' content -> processRepliesForQuestionMove oldKm newKm event content replies
    MoveAnswerEvent' content -> deleteUnwantedReplies event.entityUuid replies
    _ -> replies

deleteUnwantedReplies :: U.UUID -> [ReplyTuple] -> [ReplyTuple]
deleteUnwantedReplies entUuid = filter (\(path, _) -> not $ U.toString entUuid `L.isInfixOf` path)

processRepliesForQuestionMove :: KnowledgeModel -> KnowledgeModel -> KnowledgeModelEvent -> MoveQuestionEvent -> [ReplyTuple] -> [ReplyTuple]
processRepliesForQuestionMove oldKm newKm event content replies =
  let oldParentMap = makeParentMap oldKm
      newParentMap = makeParentMap newKm
      parentParentPath = computeParentPath oldParentMap event.parentUuid
      targetParentPath = computeParentPath newParentMap content.targetUuid
      sharedNode = computeSharedNode parentParentPath targetParentPath
      pPathDiff = takeDiffSuffix sharedNode parentParentPath
      tPathDiff = takeDiffSuffix sharedNode targetParentPath
   in doMigration newKm event.entityUuid pPathDiff tPathDiff replies

computeParentPath :: KMParentMap -> U.UUID -> [U.UUID]
computeParentPath parentMap entUuid =
  case M.lookup entUuid parentMap of
    Just parentUuid -> computeParentPath parentMap parentUuid ++ [entUuid]
    Nothing -> [entUuid]

computeSharedNode :: [U.UUID] -> [U.UUID] -> Maybe U.UUID
computeSharedNode parentParentPath targetParentPath = foldl go Nothing (reverse parentParentPath)
  where
    go Nothing nUuid =
      if nUuid `elem` targetParentPath
        then Just nUuid
        else Nothing
    go result _ = result

takeDiffSuffix :: Maybe U.UUID -> [U.UUID] -> [U.UUID]
takeDiffSuffix (Just sharedNode) = tailSafe . dropWhile (/= sharedNode)
takeDiffSuffix Nothing = Prelude.id

shouldWeMigrate :: KnowledgeModel -> [U.UUID] -> [U.UUID] -> Bool
shouldWeMigrate km pPathDiff tPathDiff = not . or . fmap (isItListQuestion km) $ pPathDiff ++ tPathDiff

doMigration :: KnowledgeModel -> U.UUID -> [U.UUID] -> [U.UUID] -> [ReplyTuple] -> [ReplyTuple]
doMigration km entUuid pPathDiff tPathDiff replies =
  if shouldWeMigrate km pPathDiff tPathDiff
    then computeDesiredPath entUuid pPathDiff tPathDiff replies
    else deleteUnwantedReplies entUuid replies

computeDesiredPath :: U.UUID -> [U.UUID] -> [U.UUID] -> [ReplyTuple] -> [ReplyTuple]
computeDesiredPath entityUuid pPathDiff tPathDiff = fmap replaceReply
  where
    replaceReply :: ReplyTuple -> ReplyTuple
    replaceReply reply@(path, value) =
      if path `replyKeyContains` entityUuid
        then
          let old =
                if null pPathDiff
                  then U.toString entityUuid
                  else f' "%s.%s" [createReplyKey pPathDiff, U.toString entityUuid]
              new =
                if null tPathDiff
                  then U.toString entityUuid
                  else f' "%s.%s" [createReplyKey tPathDiff, U.toString entityUuid]
           in (replace old new path, value)
        else reply
