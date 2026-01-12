module Wizard.Model.Plugin.PluginList where

import qualified Data.UUID as U
import GHC.Generics

data PluginList = PluginList
  { uuid :: U.UUID
  , url :: String
  , enabled :: Bool
  }
  deriving (Generic, Eq, Show)
