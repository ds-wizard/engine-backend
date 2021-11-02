module Wizard.Model.Feedback.Feedback where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Feedback =
  Feedback
    { _feedbackUuid :: U.UUID
    , _feedbackIssueId :: Int
    , _feedbackQuestionUuid :: U.UUID
    , _feedbackPackageId :: String
    , _feedbackTitle :: String
    , _feedbackContent :: String
    , _feedbackAppUuid :: U.UUID
    , _feedbackCreatedAt :: UTCTime
    , _feedbackUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq Feedback where
  a == b =
    _feedbackUuid a == _feedbackUuid b &&
    _feedbackIssueId a == _feedbackIssueId b &&
    _feedbackQuestionUuid a == _feedbackQuestionUuid b &&
    _feedbackPackageId a == _feedbackPackageId b &&
    _feedbackTitle a == _feedbackTitle b &&
    _feedbackContent a == _feedbackContent b && _feedbackAppUuid a == _feedbackAppUuid b
