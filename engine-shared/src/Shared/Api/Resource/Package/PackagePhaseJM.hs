module Shared.Api.Resource.Package.PackagePhaseJM where

import Data.Aeson
import qualified Data.Text as T
import Servant.API
import Text.Read (readMaybe)

import Shared.Model.Package.Package

instance ToJSON PackagePhase

instance FromJSON PackagePhase

instance FromHttpApiData PackagePhase where
  parseQueryParam a =
    case readMaybe (T.unpack a) of
      Just phase -> Right phase
      Nothing -> Left "Unable to parse PackagePhase"
