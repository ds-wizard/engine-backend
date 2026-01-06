module Wizard.Model.Statistics.InstanceStatistics where

import GHC.Generics

data InstanceStatistics = InstanceStatistics
  { userCount :: Int
  , pkgCount :: Int
  , prjCount :: Int
  , knowledgeModelEditorCount :: Int
  , docCount :: Int
  , tmlCount :: Int
  }
  deriving (Show, Eq, Generic)
