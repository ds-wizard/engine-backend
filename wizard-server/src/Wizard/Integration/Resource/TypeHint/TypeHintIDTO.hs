module Wizard.Integration.Resource.TypeHint.TypeHintIDTO where

import Data.Aeson
import GHC.Generics

data TypeHintLegacyIDTO = TypeHintLegacyIDTO
  { intId :: Maybe String
  , name :: String
  }
  deriving (Show, Eq, Generic)

data TypeHintIDTO = TypeHintIDTO
  { valueForSelection :: Maybe String
  , value :: String
  , raw :: Value
  }
  deriving (Show, Eq, Generic)
