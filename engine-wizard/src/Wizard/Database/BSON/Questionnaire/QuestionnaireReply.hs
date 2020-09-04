module Wizard.Database.BSON.Questionnaire.QuestionnaireReply where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import qualified Data.Map.Strict as M

import Shared.Database.BSON.Common
import Shared.Database.BSON.KnowledgeModel.KnowledgeModel ()
import Wizard.Model.Questionnaire.QuestionnaireReply

instance ToBSON (M.Map String ReplyValue) where
  toBSON = genericStringMapToBSON

instance FromBSON (M.Map String ReplyValue) where
  fromBSON = defaultStringMapFromBSON

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
      "ItemListReply" -> do
        _itemListReplyValue <- BSON.lookup "value" doc
        return ItemListReply {..}
      "IntegrationReply" -> do
        _integrationReplyValue <- BSON.lookup "value" doc
        return IntegrationReply {..}

instance FromBSON IntegrationReplyValue where
  fromBSON doc = do
    intType <- BSON.lookup "type" doc
    case intType of
      "PlainValue" -> do
        value <- BSON.lookup "value" doc
        return $ PlainValue value
      "IntegrationValue" -> do
        _integrationValueIntId <- BSON.lookup "id" doc
        _integrationValueValue <- BSON.lookup "value" doc
        return IntegrationValue {..}

-- --------------------------------------------------------------------
instance ToBSON ReplyValue where
  toBSON StringReply {..} = ["type" BSON.=: "StringReply", "value" BSON.=: _stringReplyValue]
  toBSON AnswerReply {..} = ["type" BSON.=: "AnswerReply", "value" BSON.=: _answerReplyValue]
  toBSON ItemListReply {..} = ["type" BSON.=: "ItemListReply", "value" BSON.=: _itemListReplyValue]
  toBSON IntegrationReply {..} = ["type" BSON.=: "IntegrationReply", "value" BSON.=: _integrationReplyValue]

instance ToBSON IntegrationReplyValue where
  toBSON (PlainValue value) = ["type" BSON.=: "PlainValue", "value" BSON.=: value]
  toBSON IntegrationValue {..} =
    ["type" BSON.=: "IntegrationValue", "id" BSON.=: _integrationValueIntId, "value" BSON.=: _integrationValueValue]
