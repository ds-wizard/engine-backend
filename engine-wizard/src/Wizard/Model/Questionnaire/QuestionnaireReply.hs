module Wizard.Model.Questionnaire.QuestionnaireReply where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Util.Hashable ()

type ReplyTuple = (String, Reply)

data Reply =
  Reply
    { _replyValue :: ReplyValue
    , _replyCreatedBy :: Maybe UserSuggestionDTO
    , _replyCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Hashable Reply

data ReplyValue
  = StringReply
      { _stringReplyValue :: String
      }
  | AnswerReply
      { _answerReplyValue :: U.UUID
      }
  | MultiChoiceReply
      { _multiChoiceReplyValue :: [U.UUID]
      }
  | ItemListReply
      { _itemListReplyValue :: [U.UUID]
      }
  | IntegrationReply
      { _integrationReplyValue :: IntegrationReplyType
      }
  deriving (Show, Eq, Generic)

instance Hashable ReplyValue

data IntegrationReplyType
  = PlainType
      { _plainTypeValue :: String
      }
  | IntegrationType
      { _integrationTypeIntId :: String
      , _integrationTypeValue :: String
      }
  deriving (Show, Eq, Generic)

instance Hashable IntegrationReplyType
