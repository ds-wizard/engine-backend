module Wizard.Model.BookReference.BookReference where

import Data.Time
import GHC.Generics

data BookReference = BookReference
  { shortUuid :: String
  , bookChapter :: String
  , content :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq BookReference where
  a == b =
    shortUuid a == shortUuid b
      && bookChapter a == bookChapter b
      && content a == content b
