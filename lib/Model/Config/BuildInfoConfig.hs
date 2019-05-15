module Model.Config.BuildInfoConfig where

import GHC.Generics

data BuildInfoConfig = BuildInfoConfig
  { _buildInfoConfigName :: String
  , _buildInfoConfigVersion :: String
  , _buildInfoConfigBuiltAt :: String
  } deriving (Generic, Show)
