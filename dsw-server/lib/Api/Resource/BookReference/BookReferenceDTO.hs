module Api.Resource.BookReference.BookReferenceDTO where

import Data.Time
import GHC.Generics

data BookReferenceDTO = BookReferenceDTO
  { _bookReferenceDTOShortUuid :: String
  , _bookReferenceDTOBookChapter :: String
  , _bookReferenceDTOContent :: String
  , _bookReferenceDTOCreatedAt :: UTCTime
  , _bookReferenceDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq BookReferenceDTO where
  a == b =
    _bookReferenceDTOShortUuid a == _bookReferenceDTOShortUuid b &&
    _bookReferenceDTOContent a == _bookReferenceDTOContent b
