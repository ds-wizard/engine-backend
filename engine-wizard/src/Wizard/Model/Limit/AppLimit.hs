module Wizard.Model.Limit.AppLimit where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data AppLimit =
  AppLimit
    { _appLimitUuid :: U.UUID
    , _appLimitUsers :: Maybe Int
    , _appLimitActiveUsers :: Maybe Int
    , _appLimitKnowledgeModels :: Maybe Int
    , _appLimitBranches :: Maybe Int
    , _appLimitTemplates :: Maybe Int
    , _appLimitQuestionnaires :: Maybe Int
    , _appLimitDocuments :: Maybe Int
    , _appLimitStorage :: Maybe Int64
    , _appLimitCreatedAt :: UTCTime
    , _appLimitUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
