module Wizard.Api.Resource.App.AppDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data AppDTO = AppDTO
  { uuid :: U.UUID
  , appId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , enabled :: Bool
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
