module Wizard.Api.Resource.App.AppDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data AppDTO =
  AppDTO
    { _appDTOUuid :: U.UUID
    , _appDTOAppId :: String
    , _appDTOName :: String
    , _appDTOServerDomain :: String
    , _appDTOServerUrl :: String
    , _appDTOClientUrl :: String
    , _appDTOEnabled :: Bool
    , _appDTOLogoUrl :: Maybe String
    , _appDTOPrimaryColor :: Maybe String
    , _appDTOCreatedAt :: UTCTime
    , _appDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
