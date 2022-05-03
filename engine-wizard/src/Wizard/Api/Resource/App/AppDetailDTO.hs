module Wizard.Api.Resource.App.AppDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Plan.AppPlan

data AppDetailDTO =
  AppDetailDTO
    { _appDetailDTOUuid :: U.UUID
    , _appDetailDTOAppId :: String
    , _appDetailDTOName :: String
    , _appDetailDTOServerDomain :: String
    , _appDetailDTOServerUrl :: String
    , _appDetailDTOClientUrl :: String
    , _appDetailDTOEnabled :: Bool
    , _appDetailDTOLogoUrl :: Maybe String
    , _appDetailDTOPrimaryColor :: Maybe String
    , _appDetailDTOPlans :: [AppPlan]
    , _appDetailDTOUsage :: UsageDTO
    , _appDetailDTOUsers :: [UserDTO]
    , _appDetailDTOCreatedAt :: UTCTime
    , _appDetailDTOUpdatedAt :: UTCTime
    }
  deriving (Generic)
