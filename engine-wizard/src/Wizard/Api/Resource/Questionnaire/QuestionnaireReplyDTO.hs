module Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO where

import qualified Data.UUID as U
import GHC.Generics

data ReplyValueDTO
  = StringReplyDTO
      { _stringReplyDTOValue :: String
      }
  | AnswerReplyDTO
      { _answerReplyDTOValue :: U.UUID
      }
  | MultiChoiceReplyDTO
      { _multiChoiceReplyDTOValue :: [U.UUID]
      }
  | ItemListReplyDTO
      { _itemListReplyDTOValue :: [U.UUID]
      }
  | IntegrationReplyDTO
      { _integrationReplyDTOValue :: IntegrationReplyValueDTO
      }
  deriving (Show, Eq, Generic)

data IntegrationReplyValueDTO
  = PlainValueDTO
      { _plainValueDTOValue :: String
      }
  | IntegrationValueDTO
      { _integrationValueDTOIntId :: String
      , _integrationValueDTOValue :: String
      }
  deriving (Show, Eq, Generic)
