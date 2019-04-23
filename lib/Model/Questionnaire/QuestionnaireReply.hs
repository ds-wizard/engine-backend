module Model.Questionnaire.QuestionnaireReply where

import qualified Data.UUID as U
import GHC.Generics

data Reply = Reply
  { _replyPath :: String
  , _replyValue :: ReplyValue
  } deriving (Show, Eq, Generic)

data ReplyValue
  = StringReply { _stringReplyValue :: String }
  | AnswerReply { _answerReplyValue :: U.UUID }
  | ItemListReply { _itemListReplyValue :: Int }
  | IntegrationReply { _integrationReplyValue :: IntegrationReplyValue }
  deriving (Show, Eq, Generic)

data IntegrationReplyValue
  = PlainValue String
  | IntegrationValue { _integrationValueIntId :: String
                     , _integrationValueIntValue :: String }
  deriving (Show, Eq, Generic)
