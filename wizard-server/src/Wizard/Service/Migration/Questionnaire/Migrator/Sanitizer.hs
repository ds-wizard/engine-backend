module Wizard.Service.Migration.Questionnaire.Migrator.Sanitizer (
  sanitizeQuestionnaireEvents,
) where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import qualified Wizard.Service.Migration.Questionnaire.Migrator.ChangeQTypeSanitizer as CTS
import qualified Wizard.Service.Migration.Questionnaire.Migrator.MoveSanitizer as MS
import Wizard.Service.Questionnaire.Compiler.CompilerService
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

sanitizeQuestionnaireEvents :: U.UUID -> KnowledgeModel -> KnowledgeModel -> [QuestionnaireEvent] -> AppContextM [QuestionnaireEvent]
sanitizeQuestionnaireEvents qtnUuid oldKm newKm events = do
  oldQtnContent <- compileQuestionnairePreview events
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

generateClearReplyEvents :: U.UUID -> M.Map String Reply -> M.Map String Reply -> AppContextM [QuestionnaireEvent]
generateClearReplyEvents qtnUuid oldReplies sanitizedReplies = traverse generateEvent repliesToBeDeleted
  where
    repliesToBeDeleted :: [ReplyTuple]
    repliesToBeDeleted = M.toList . M.filterWithKey (\k _ -> k `M.notMember` sanitizedReplies) $ oldReplies
    generateEvent :: ReplyTuple -> AppContextM QuestionnaireEvent
    generateEvent (k, _) = do
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      tenantUuid <- asks currentTenantUuid
      return . ClearReplyEvent' $ ClearReplyEvent eUuid k qtnUuid tenantUuid (Just user.uuid) now

generateSetReplyEvents :: U.UUID -> M.Map String Reply -> M.Map String Reply -> AppContextM [QuestionnaireEvent]
generateSetReplyEvents qtnUuid oldReplies sanitizedReplies = foldl generateEvent (return []) (M.toList sanitizedReplies)
  where
    generateEvent :: AppContextM [QuestionnaireEvent] -> ReplyTuple -> AppContextM [QuestionnaireEvent]
    generateEvent accM (keyFromSanitizedReply, valueFromSanitizedReply) = do
      acc <- accM
      eUuid <- liftIO generateUuid
      now <- liftIO getCurrentTime
      user <- getCurrentUser
      tenantUuid <- asks currentTenantUuid
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
                          qtnUuid
                          tenantUuid
                          (Just user.uuid)
                          now
                     ]
          Nothing ->
            acc
              ++ [ SetReplyEvent' $
                    SetReplyEvent eUuid keyFromSanitizedReply valueFromSanitizedReply.value qtnUuid tenantUuid (Just user.uuid) now
                 ]
