module Wizard.Model.Questionnaire.QuestionnaireComment where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireCommentThread = QuestionnaireCommentThread
  { uuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , comments :: [QuestionnaireComment]
  , private :: Bool
  , questionnaireUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data QuestionnaireComment = QuestionnaireComment
  { uuid :: U.UUID
  , text :: String
  , threadUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
