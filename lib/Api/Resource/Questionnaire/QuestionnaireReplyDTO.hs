module Api.Resource.Questionnaire.QuestionnaireReplyDTO where

import qualified Data.UUID as U
import GHC.Generics

data ReplyDTO = ReplyDTO
  { _replyDTOPath :: String
  , _replyDTOValue :: ReplyValueDTO
  } deriving (Show, Eq, Generic)

data ReplyValueDTO
  = StringReplyDTO { _stringReplyDTOValue :: String }
  | AnswerReplyDTO { _answerReplyDTOValue :: U.UUID }
  | ItemListReplyDTO { _itemListReplyDTOValue :: Int }
  | IntegrationReplyDTO { _integrationReplyDTOValue :: IntegrationReplyValueDTO }
  deriving (Show, Eq, Generic)

data IntegrationReplyValueDTO
  = PlainValueDTO String
  | IntegrationValueDTO { _integrationValueDTOIntId :: String
                        , _integrationValueDTOIntValue :: String }
  deriving (Show, Eq, Generic)
