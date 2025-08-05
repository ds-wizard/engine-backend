module Wizard.Model.Questionnaire.QuestionnaireReply where

import Data.Aeson ()
import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Util.Hashable ()
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

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
  | ItemSelectReply
      { isValue :: U.UUID
      }
  | FileReply
      { fValue :: U.UUID
      }
  deriving (Show, Eq, Generic)

instance Hashable ReplyValue

data IntegrationReplyType
  = PlainType
      { value :: String
      }
  | IntegrationLegacyType
      { intId :: Maybe String
      , value :: String
      }
  | IntegrationType
      { value :: String
      , raw :: String
      }
  deriving (Show, Eq, Generic)

instance Hashable IntegrationReplyType
