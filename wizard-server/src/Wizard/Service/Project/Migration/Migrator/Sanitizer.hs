module Wizard.Service.Project.Migration.Migrator.Sanitizer (
  sanitizeProjectEvents,
) where

import Control.Monad.Reader (liftIO)
import qualified Data.Map.Strict as M
import Data.Time

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectReply
import Wizard.Service.Project.Compiler.ProjectCompilerService
import qualified Wizard.Service.Project.Migration.Migrator.ChangeQTypeSanitizer as CTS
import qualified Wizard.Service.Project.Migration.Migrator.MoveSanitizer as MS
import Wizard.Service.User.UserMapper

sanitizeProjectEvents :: KnowledgeModel -> KnowledgeModel -> [ProjectEventList] -> AppContextM [ProjectEventList]
sanitizeProjectEvents oldKm newKm events = do
  let oldProjectContent = compileProjectEvents events
  let oldReplies = oldProjectContent.replies
  now <- liftIO getCurrentTime
  let sanitizedReplies = M.fromList . sanitizeReplies now oldKm newKm . M.toList $ oldReplies
  clearReplyEvents <- generateClearReplyEvents oldReplies sanitizedReplies
  setReplyEvents <- generateSetReplyEvents oldReplies sanitizedReplies
  return $ clearReplyEvents ++ setReplyEvents

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeReplies :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies now oldKm newKm = MS.sanitizeReplies now oldKm newKm . CTS.sanitizeReplies newKm

generateClearReplyEvents :: M.Map String Reply -> M.Map String Reply -> AppContextM [ProjectEventList]
generateClearReplyEvents oldReplies sanitizedReplies = traverse generateEvent repliesToBeDeleted
  where
    repliesToBeDeleted :: [ReplyTuple]
    repliesToBeDeleted = M.toList . M.filterWithKey (\k _ -> k `M.notMember` sanitizedReplies) $ oldReplies
    generateEvent :: ReplyTuple -> AppContextM ProjectEventList
    generateEvent (k, _) = do
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      return . ClearReplyEventList' $ ClearReplyEventList eUuid k (Just (toSuggestion' user)) now

generateSetReplyEvents :: M.Map String Reply -> M.Map String Reply -> AppContextM [ProjectEventList]
generateSetReplyEvents oldReplies sanitizedReplies = foldl generateEvent (return []) (M.toList sanitizedReplies)
  where
    generateEvent :: AppContextM [ProjectEventList] -> ReplyTuple -> AppContextM [ProjectEventList]
    generateEvent accM (keyFromSanitizedReply, valueFromSanitizedReply) = do
      acc <- accM
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      return $
        case M.lookup keyFromSanitizedReply oldReplies of
          Just valueFromOldReply ->
            if valueFromOldReply.value == valueFromSanitizedReply.value
              then acc
              else
                acc
                  ++ [ SetReplyEventList' $
                         SetReplyEventList
                           eUuid
                           keyFromSanitizedReply
                           valueFromSanitizedReply.value
                           (Just (toSuggestion' user))
                           now
                     ]
          Nothing ->
            acc
              ++ [ SetReplyEventList' $ SetReplyEventList eUuid keyFromSanitizedReply valueFromSanitizedReply.value (Just (toSuggestion' user)) now
                 ]
