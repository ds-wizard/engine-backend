module Wizard.Database.BSON.Questionnaire.QuestionnaireReply where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Questionnaire.QuestionnaireReply

instance FromBSON ReplyValue where
  fromBSON doc = do
    rvType <- BSON.lookup "type" doc
    case rvType of
      "StringReply" -> do
        _stringReplyValue <- BSON.lookup "value" doc
        return StringReply {..}
      "AnswerReply" -> do
        _answerReplyValue <- BSON.lookup "value" doc
        return AnswerReply {..}
      "MultiChoiceReply" -> do
        _multiChoiceReplyValue <- BSON.lookup "value" doc
        return MultiChoiceReply {..}
      "ItemListReply" -> do
        _itemListReplyValue <- BSON.lookup "value" doc
        return ItemListReply {..}
      "IntegrationReply" -> do
        _integrationReplyValue <- BSON.lookup "value" doc
        return IntegrationReply {..}

instance FromBSON IntegrationReplyType where
  fromBSON doc = do
    intType <- BSON.lookup "type" doc
    case intType of
      "PlainType" -> do
        value <- BSON.lookup "value" doc
        return $ PlainType value
      "IntegrationType" -> do
        _integrationTypeIntId <- BSON.lookup "id" doc
        _integrationTypeValue <- BSON.lookup "value" doc
        return IntegrationType {..}

-- --------------------------------------------------------------------
instance ToBSON ReplyValue where
  toBSON StringReply {..} = ["type" BSON.=: "StringReply", "value" BSON.=: _stringReplyValue]
  toBSON AnswerReply {..} = ["type" BSON.=: "AnswerReply", "value" BSON.=: _answerReplyValue]
  toBSON MultiChoiceReply {..} = ["type" BSON.=: "MultiChoiceReply", "value" BSON.=: _multiChoiceReplyValue]
  toBSON ItemListReply {..} = ["type" BSON.=: "ItemListReply", "value" BSON.=: _itemListReplyValue]
  toBSON IntegrationReply {..} = ["type" BSON.=: "IntegrationReply", "value" BSON.=: _integrationReplyValue]

instance ToBSON IntegrationReplyType where
  toBSON (PlainType value) = ["type" BSON.=: "PlainType", "value" BSON.=: value]
  toBSON IntegrationType {..} =
    ["type" BSON.=: "IntegrationType", "id" BSON.=: _integrationTypeIntId, "value" BSON.=: _integrationTypeValue]
