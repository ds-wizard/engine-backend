module Wizard.Service.Migration.Questionnaire.Migrator.ChangeQTypeSanitizator where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.String (splitOn)
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Util.Maybe (concatMaybe)

sanitizeReplies :: KnowledgeModel -> [ReplyTuple] -> [ReplyTuple]
sanitizeReplies km = mapMaybe (sanitizeReply km)

sanitizeReply :: KnowledgeModel -> ReplyTuple -> Maybe ReplyTuple
sanitizeReply km (path, reply) =
  let pathParsed = reverse $ splitOn "." path
   in case sanitizeQuestion km pathParsed reply.value of
        Just replyValue -> Just (path, reply {value = replyValue})
        Nothing -> Nothing

-- --------------------------------
-- PRIVATE
-- --------------------------------
sanitizeQuestion :: KnowledgeModel -> [String] -> ReplyValue -> Maybe ReplyValue
sanitizeQuestion km (questionUuidS : _) replyValue =
  case concatMaybe $ fmap (\qUuid -> M.lookup qUuid (getQuestionsM km)) . U.fromString $ questionUuidS of
    (Just (OptionsQuestion' q)) -> sanitizeOptionsQuestion km replyValue q
    (Just (MultiChoiceQuestion' q)) -> sanitizeMultiChoiceQuestion km replyValue q
    (Just (ListQuestion' q)) -> sanitizeListQuestion km replyValue q
    (Just (ValueQuestion' q)) -> sanitizeValueQuestion km replyValue q
    (Just (IntegrationQuestion' q)) -> sanitizeIntegrationQuestion km replyValue q
    _ -> Nothing

sanitizeOptionsQuestion :: KnowledgeModel -> ReplyValue -> OptionsQuestion -> Maybe ReplyValue
sanitizeOptionsQuestion km AnswerReply {..} q =
  if aValue `elem` q.answerUuids
    then Just $ AnswerReply {..}
    else Nothing
sanitizeOptionsQuestion _ _ _ = Nothing

sanitizeMultiChoiceQuestion :: KnowledgeModel -> ReplyValue -> MultiChoiceQuestion -> Maybe ReplyValue
sanitizeMultiChoiceQuestion km MultiChoiceReply {..} q =
  let newValue = filter (`elem` q.choiceUuids) mcValue
   in Just $ MultiChoiceReply {mcValue = newValue}
sanitizeMultiChoiceQuestion _ _ _ = Nothing

sanitizeListQuestion :: KnowledgeModel -> ReplyValue -> ListQuestion -> Maybe ReplyValue
sanitizeListQuestion km ItemListReply {..} q = Just $ ItemListReply {..}
sanitizeListQuestion _ _ _ = Nothing

sanitizeValueQuestion :: KnowledgeModel -> ReplyValue -> ValueQuestion -> Maybe ReplyValue
sanitizeValueQuestion km StringReply {..} q = Just $ StringReply {..}
sanitizeValueQuestion km IntegrationReply {iValue = replyValue} q =
  case replyValue of
    PlainType value -> Just $ StringReply {sValue = value}
    IntegrationType {..} -> Just $ StringReply {sValue = value}
sanitizeValueQuestion _ _ _ = Nothing

sanitizeIntegrationQuestion :: KnowledgeModel -> ReplyValue -> IntegrationQuestion -> Maybe ReplyValue
sanitizeIntegrationQuestion km IntegrationReply {..} q = Just $ IntegrationReply {..}
sanitizeIntegrationQuestion km StringReply {..} q =
  Just $ IntegrationReply {iValue = PlainType sValue}
sanitizeIntegrationQuestion _ _ _ = Nothing
