module Wizard.Service.Questionnaire.Migration.Migrator.Sanitizer (
  sanitizeQuestionnaireEvents,
) where

import Control.Monad.Reader (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventList
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Questionnaire.Compiler.CompilerService
import qualified Wizard.Service.Questionnaire.Migration.Migrator.ChangeQTypeSanitizer as CTS
import qualified Wizard.Service.Questionnaire.Migration.Migrator.MoveSanitizer as MS
import Wizard.Service.User.UserMapper

sanitizeQuestionnaireEvents :: U.UUID -> KnowledgeModel -> KnowledgeModel -> [QuestionnaireEventList] -> AppContextM [QuestionnaireEventList]
sanitizeQuestionnaireEvents qtnUuid oldKm newKm events = do
  let oldQtnContent = compileQuestionnaire events
  let oldReplies = oldQtnContent.replies
  now <- liftIO getCurrentTime
  let sanitizedReplies = M.fromList . sanitizeReplies now oldKm newKm . M.toList $ oldReplies
  clearReplyEvents <- generateClearReplyEvents qtnUuid oldReplies sanitizedReplies
  setReplyEvents <- generateSetReplyEvents qtnUuid oldReplies sanitizedReplies
  return $ events ++ clearReplyEvents ++ setReplyEvents

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeReplies :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies now oldKm newKm = MS.sanitizeReplies now oldKm newKm . CTS.sanitizeReplies newKm

generateClearReplyEvents :: U.UUID -> M.Map String Reply -> M.Map String Reply -> AppContextM [QuestionnaireEventList]
generateClearReplyEvents qtnUuid oldReplies sanitizedReplies = traverse generateEvent repliesToBeDeleted
  where
    repliesToBeDeleted :: [ReplyTuple]
    repliesToBeDeleted = M.toList . M.filterWithKey (\k _ -> k `M.notMember` sanitizedReplies) $ oldReplies
    generateEvent :: ReplyTuple -> AppContextM QuestionnaireEventList
    generateEvent (k, _) = do
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      return . ClearReplyEventList' $ ClearReplyEventList eUuid k (Just (toSuggestion' user)) now

generateSetReplyEvents :: U.UUID -> M.Map String Reply -> M.Map String Reply -> AppContextM [QuestionnaireEventList]
generateSetReplyEvents qtnUuid oldReplies sanitizedReplies = foldl generateEvent (return []) (M.toList sanitizedReplies)
  where
    generateEvent :: AppContextM [QuestionnaireEventList] -> ReplyTuple -> AppContextM [QuestionnaireEventList]
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
