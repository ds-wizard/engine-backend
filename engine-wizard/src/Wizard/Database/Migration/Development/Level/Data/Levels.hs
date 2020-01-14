module Wizard.Database.Migration.Development.Level.Data.Levels where

import Data.Maybe (fromJust)
import Data.Time

import Wizard.Model.Level.Level

level1 :: Level
level1 =
  Level
    { _levelLevel = 1
    , _levelTitle = "Before Submitting the Proposal"
    , _levelDescription = Just ""
    , _levelCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _levelUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

level2 :: Level
level2 =
  Level
    { _levelLevel = 2
    , _levelTitle = "Before Submitting the DMP"
    , _levelDescription = Just ""
    , _levelCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _levelUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

level3 :: Level
level3 =
  Level
    { _levelLevel = 3
    , _levelTitle = "Before Finishing the Project"
    , _levelDescription = Just ""
    , _levelCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _levelUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
