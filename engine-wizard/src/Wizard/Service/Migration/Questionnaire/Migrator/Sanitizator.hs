module Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator (
  sanitizeQuestionnaireEvents,
) where

import Control.Monad.Reader (liftIO)
import qualified Data.Map.Strict as M
import Data.Time

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import qualified Wizard.Service.Migration.Questionnaire.Migrator.ChangeQTypeSanitizator as CTS
import qualified Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizator as MS
import Wizard.Service.Questionnaire.Compiler.CompilerService

sanitizeQuestionnaireEvents
  :: KnowledgeModel -> KnowledgeModel -> [QuestionnaireEvent] -> AppContextM [QuestionnaireEvent]
sanitizeQuestionnaireEvents oldKm newKm events = do
  oldQtnContent <- compileQuestionnairePreview events
  let oldReplies = oldQtnContent.replies
  now <- liftIO getCurrentTime
  let sanitizedReplies = M.fromList . sanitizeReplies now oldKm newKm . M.toList $ oldReplies
  clearReplyEvents <- generateClearReplyEvents oldReplies sanitizedReplies
  setReplyEvents <- generateSetReplyEvents oldReplies sanitizedReplies
  return $ events ++ clearReplyEvents ++ setReplyEvents

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeReplies :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies now oldKm newKm = MS.sanitizeReplies now oldKm newKm . CTS.sanitizeReplies newKm

generateClearReplyEvents :: M.Map String Reply -> M.Map String Reply -> AppContextM [QuestionnaireEvent]
generateClearReplyEvents oldReplies sanitizedReplies = traverse generateEvent repliesToBeDeleted
  where
    repliesToBeDeleted :: [ReplyTuple]
    repliesToBeDeleted = M.toList . M.filterWithKey (\k _ -> k `M.notMember` sanitizedReplies) $ oldReplies
    generateEvent :: ReplyTuple -> AppContextM QuestionnaireEvent
    generateEvent (k, _) = do
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      return . ClearReplyEvent' $ ClearReplyEvent eUuid k (Just user.uuid) now

generateSetReplyEvents :: M.Map String Reply -> M.Map String Reply -> AppContextM [QuestionnaireEvent]
generateSetReplyEvents oldReplies sanitizedReplies = foldl generateEvent (return []) (M.toList sanitizedReplies)
  where
    generateEvent :: AppContextM [QuestionnaireEvent] -> ReplyTuple -> AppContextM [QuestionnaireEvent]
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
                  ++ [ SetReplyEvent' $
                        SetReplyEvent
                          eUuid
                          keyFromSanitizedReply
                          valueFromSanitizedReply.value
                          (Just user.uuid)
                          now
                     ]
          Nothing ->
            acc
              ++ [ SetReplyEvent' $
                    SetReplyEvent eUuid keyFromSanitizedReply valueFromSanitizedReply.value (Just user.uuid) now
                 ]
