module Wizard.Api.Resource.Feedback.FeedbackDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data FeedbackDTO = FeedbackDTO
  { uuid :: U.UUID
  , issueId :: Int
  , issueUrl :: String
  , questionUuid :: U.UUID
  , knowledgeModelPackageId :: String
  , title :: String
  , content :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq FeedbackDTO where
  a == b =
    uuid a == uuid b
      && issueId a == issueId b
      && issueUrl a == issueUrl b
      && questionUuid a == questionUuid b
      && knowledgeModelPackageId a == knowledgeModelPackageId b
      && title a == title b
      && content a == content b
