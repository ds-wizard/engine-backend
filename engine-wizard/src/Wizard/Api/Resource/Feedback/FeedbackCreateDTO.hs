module Wizard.Api.Resource.Feedback.FeedbackCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data FeedbackCreateDTO = FeedbackCreateDTO
  { questionUuid :: U.UUID
  , packageId :: String
  , title :: String
  , content :: String
  }
  deriving (Show, Eq, Generic)
