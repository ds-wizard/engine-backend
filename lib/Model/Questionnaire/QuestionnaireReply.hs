module Model.Questionnaire.QuestionnaireReply where

import qualified Data.UUID as U
import GHC.Generics

data Reply = Reply
  { _replyPath :: String
  , _replyValue :: ReplyValue
  } deriving (Show, Eq)

data ReplyValue
  = StringReply { _stringReplyValue :: String }
  | AnswerReply { _answerReplyValue :: U.UUID }
  | ItemListReply { _itemListReplyValue :: Int }
  | IntegrationReply { _integrationReplyValue :: IntegrationReplyValue }
  deriving (Generic, Show, Eq)

data IntegrationReplyValue =
  FairsharingIntegrationReply' FairsharingIntegrationReply
  deriving (Show, Eq)

data FairsharingIntegrationReply = FairsharingIntegrationReply
  { _fairsharingIntegrationReplyIntId :: String
  , _fairsharingIntegrationReplyName :: String
  } deriving (Show, Eq)
