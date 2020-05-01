module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM where

import Control.Monad
import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO

instance FromJSON ReplyDTO where
  parseJSON = genericParseJSON simpleOptions

instance FromJSON ReplyValueDTO where
  parseJSON (Object o) = do
    rvType <- o .: "type"
    case rvType of
      "StringReply" -> do
        _stringReplyDTOValue <- o .: "value"
        return StringReplyDTO {..}
      "AnswerReply" -> do
        _answerReplyDTOValue <- o .: "value"
        return AnswerReplyDTO {..}
      "ItemListReply" -> do
        _itemListReplyDTOValue <- o .: "value"
        return ItemListReplyDTO {..}
      "IntegrationReply" -> do
        _integrationReplyDTOValue <- o .: "value"
        return IntegrationReplyDTO {..}
      _ -> fail "One of the replies has unsupported reply type"
  parseJSON _ = mzero

instance FromJSON IntegrationReplyValueDTO where
  parseJSON (Object o) = do
    intType <- o .: "type"
    case intType of
      "PlainValue" -> do
        value <- o .: "value"
        return $ PlainValueDTO value
      "IntegrationValue" -> do
        _integrationValueDTOIntId <- o .: "id"
        _integrationValueDTOIntValue <- o .: "value"
        return IntegrationValueDTO {..}
  parseJSON _ = mzero

-- --------------------------------------------------------------------
instance ToJSON ReplyDTO where
  toJSON = genericToJSON simpleOptions

instance ToJSON ReplyValueDTO where
  toJSON StringReplyDTO {..} = object ["type" .= "StringReply", "value" .= _stringReplyDTOValue]
  toJSON AnswerReplyDTO {..} = object ["type" .= "AnswerReply", "value" .= _answerReplyDTOValue]
  toJSON ItemListReplyDTO {..} = object ["type" .= "ItemListReply", "value" .= _itemListReplyDTOValue]
  toJSON IntegrationReplyDTO {..} = object ["type" .= "IntegrationReply", "value" .= _integrationReplyDTOValue]

instance ToJSON IntegrationReplyValueDTO where
  toJSON (PlainValueDTO value) = object ["type" .= "PlainValue", "value" .= value]
  toJSON IntegrationValueDTO {..} =
    object ["type" .= "IntegrationValue", "id" .= _integrationValueDTOIntId, "value" .= _integrationValueDTOIntValue]
