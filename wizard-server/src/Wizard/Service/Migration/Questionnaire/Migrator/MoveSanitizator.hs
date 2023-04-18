module Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizator where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Records
import Prelude hiding (id)
import qualified Prelude

import Shared.Common.Util.List (tailSafe)
import Shared.Common.Util.String (f', replace)
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Migration.Questionnaire.Migrator.MoveEventGenerator
import WizardLib.Common.Model.Questionnaire.QuestionnaireUtil
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelUtil

sanitizeReplies :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies now oldKm newKm replies = sanitizeRepliesWithEvents oldKm newKm replies (generateEvents now oldKm newKm)

sanitizeRepliesWithEvents :: KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [Event] -> [ReplyTuple]
sanitizeRepliesWithEvents oldKm newKm = foldl (processReplies oldKm newKm)

-- --------------------------------
-- PRIVATE
-- --------------------------------
processReplies :: KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> Event -> [ReplyTuple]
processReplies oldKm newKm replies (MoveAnswerEvent' event) = deleteUnwantedReplies event.entityUuid replies
processReplies oldKm newKm replies (MoveQuestionEvent' event) = processRepliesForQuestionMove oldKm newKm event replies
processReplies _ _ replies _ = replies

deleteUnwantedReplies :: U.UUID -> [ReplyTuple] -> [ReplyTuple]
deleteUnwantedReplies entUuid = filter (\(path, _) -> not $ U.toString entUuid `L.isInfixOf` path)

processRepliesForQuestionMove
  :: (HasField "parentUuid" event U.UUID, HasField "targetUuid" event U.UUID, HasField "entityUuid" event U.UUID)
  => KnowledgeModel
  -> KnowledgeModel
  -> event
  -> [ReplyTuple]
  -> [ReplyTuple]
processRepliesForQuestionMove oldKm newKm event replies =
  let oldParentMap = makeParentMap oldKm
      newParentMap = makeParentMap newKm
      parentParentPath = computeParentPath oldParentMap event.parentUuid
      targetParentPath = computeParentPath newParentMap event.targetUuid
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
