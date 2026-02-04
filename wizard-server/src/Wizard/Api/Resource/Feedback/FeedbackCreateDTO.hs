module Wizard.Api.Resource.Feedback.FeedbackCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data FeedbackCreateDTO = FeedbackCreateDTO
  { questionUuid :: U.UUID
  , knowledgeModelPackageUuid :: U.UUID
  , title :: String
  , content :: String
  }
  deriving (Show, Eq, Generic)
