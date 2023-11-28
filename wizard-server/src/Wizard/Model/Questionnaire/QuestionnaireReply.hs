module Wizard.Model.Questionnaire.QuestionnaireReply where

import Data.Aeson ()
import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Util.Hashable ()

type ReplyTuple = (String, Reply)

data Reply = Reply
  { value :: ReplyValue
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable Reply

data ReplyValue
  = StringReply
      { sValue :: String
      }
  | AnswerReply
      { aValue :: U.UUID
      }
  | MultiChoiceReply
      { mcValue :: [U.UUID]
      }
  | ItemListReply
      { ilValue :: [U.UUID]
      }
  | IntegrationReply
      { iValue :: IntegrationReplyType
      }
  deriving (Show, Eq, Generic)

instance Hashable ReplyValue

data IntegrationReplyType
  = PlainType
      { value :: String
      }
  | IntegrationType
      { intId :: Maybe String
      , value :: String
      }
  deriving (Show, Eq, Generic)

instance Hashable IntegrationReplyType
