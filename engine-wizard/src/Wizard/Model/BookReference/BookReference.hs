module Wizard.Model.BookReference.BookReference where

import Data.Time
import GHC.Generics

data BookReference =
  BookReference
    { _bookReferenceShortUuid :: String
    , _bookReferenceBookChapter :: String
    , _bookReferenceContent :: String
    , _bookReferenceCreatedAt :: UTCTime
    , _bookReferenceUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq BookReference where
  a == b =
    _bookReferenceShortUuid a == _bookReferenceShortUuid b &&
    _bookReferenceBookChapter a == _bookReferenceBookChapter b && _bookReferenceContent a == _bookReferenceContent b
