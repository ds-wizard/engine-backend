module Service.Migration.Questionnaire.Sanitizator where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.Questionnaire.QuestionnaireReply
import Util.Maybe (concatMaybe)
import Util.String (splitOn)

sanitizeReplies :: KnowledgeModel -> [Reply] -> [Reply]
sanitizeReplies km replies = catMaybes . fmap (sanitizeReply km) $ replies

sanitizeReply :: KnowledgeModel -> Reply -> Maybe Reply
sanitizeReply km reply =
  let pathParsed = reverse $ splitOn "." (reply ^. path)
  in case sanitizeQuestion km pathParsed (reply ^. value) of
       Just replyValue -> Just $ reply & value .~ replyValue
       Nothing -> Nothing

-- -------------------------------------------------------------
sanitizeQuestion :: KnowledgeModel -> [String] -> ReplyValue -> Maybe ReplyValue
sanitizeQuestion km (questionUuidS:_) replyValue =
  case concatMaybe $ fmap (\qUuid -> M.lookup qUuid (km ^. questionsM)) . U.fromString $ questionUuidS of
    (Just (OptionsQuestion' q)) -> sanitizeOptionsQuestion km replyValue q
    (Just (ListQuestion' q)) -> sanitizeListQuestion km replyValue q
    (Just (ValueQuestion' q)) -> sanitizeValueQuestion km replyValue q
    (Just (IntegrationQuestion' q)) -> sanitizeIntegrationQuestion km replyValue q
    _ -> Nothing

sanitizeOptionsQuestion :: KnowledgeModel -> ReplyValue -> OptionsQuestion -> Maybe ReplyValue
sanitizeOptionsQuestion km AnswerReply {..} q =
  if _answerReplyValue `elem` (q ^. answerUuids)
    then Just $ AnswerReply {..}
    else Nothing
sanitizeOptionsQuestion _ _ _ = Nothing

sanitizeListQuestion :: KnowledgeModel -> ReplyValue -> ListQuestion -> Maybe ReplyValue
sanitizeListQuestion km ItemListReply {..} q = Just $ ItemListReply {..}
sanitizeListQuestion _ _ _ = Nothing

sanitizeValueQuestion :: KnowledgeModel -> ReplyValue -> ValueQuestion -> Maybe ReplyValue
sanitizeValueQuestion km StringReply {..} q = Just $ StringReply {..}
sanitizeValueQuestion km IntegrationReply {_integrationReplyValue = replyValue} q =
  case replyValue of
    PlainValue value -> Just $ StringReply {_stringReplyValue = value}
    IntegrationValue {..} -> Just $ StringReply {_stringReplyValue = _integrationValueIntValue}
sanitizeValueQuestion _ _ _ = Nothing

sanitizeIntegrationQuestion :: KnowledgeModel -> ReplyValue -> IntegrationQuestion -> Maybe ReplyValue
sanitizeIntegrationQuestion km IntegrationReply {..} q = Just $ IntegrationReply {..}
sanitizeIntegrationQuestion km StringReply {..} q =
  Just $ IntegrationReply {_integrationReplyValue = PlainValue _stringReplyValue}
sanitizeIntegrationQuestion _ _ _ = Nothing
