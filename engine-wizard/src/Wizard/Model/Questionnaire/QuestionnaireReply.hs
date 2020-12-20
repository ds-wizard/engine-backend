module Wizard.Model.Questionnaire.QuestionnaireReply where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

type Reply = (String, ReplyValue)

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
      { _integrationReplyValue :: IntegrationReplyValue
      }
  deriving (Show, Eq, Generic)

instance Hashable ReplyValue

data IntegrationReplyValue
  = PlainValue
      { _plainValueValue :: String
      }
  | IntegrationValue
      { _integrationValueIntId :: String
      , _integrationValueValue :: String
      }
  deriving (Show, Eq, Generic)

instance Hashable IntegrationReplyValue
