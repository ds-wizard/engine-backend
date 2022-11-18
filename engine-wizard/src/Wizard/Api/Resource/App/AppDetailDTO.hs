module Wizard.Api.Resource.App.AppDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Plan.AppPlan

data AppDetailDTO = AppDetailDTO
  { uuid :: U.UUID
  , appId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , enabled :: Bool
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , plans :: [AppPlan]
  , usage :: UsageDTO
  , users :: [UserDTO]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic)
