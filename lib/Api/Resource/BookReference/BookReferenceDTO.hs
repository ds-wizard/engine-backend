module Api.Resource.BookReference.BookReferenceDTO where

import Control.Monad
import Data.Aeson
import Data.Time

data BookReferenceDTO = BookReferenceDTO
  { _bookReferenceDTOShortUuid :: String
  , _bookReferenceDTOBookChapter :: String
  , _bookReferenceDTOContent :: String
  , _bookReferenceDTOCreatedAt :: UTCTime
  , _bookReferenceDTOUpdatedAt :: UTCTime
  } deriving (Show)

instance Eq BookReferenceDTO where
  a == b =
    _bookReferenceDTOShortUuid a == _bookReferenceDTOShortUuid b &&
    _bookReferenceDTOContent a == _bookReferenceDTOContent b

instance FromJSON BookReferenceDTO where
  parseJSON (Object o) = do
    _bookReferenceDTOShortUuid <- o .: "shortUuid"
    _bookReferenceDTOBookChapter <- o .: "bookChapter"
    _bookReferenceDTOContent <- o .: "content"
    _bookReferenceDTOCreatedAt <- o .: "createdAt"
    _bookReferenceDTOUpdatedAt <- o .: "updatedAt"
    return BookReferenceDTO {..}
  parseJSON _ = mzero

instance ToJSON BookReferenceDTO where
  toJSON BookReferenceDTO {..} =
    object
      [ "shortUuid" .= _bookReferenceDTOShortUuid
      , "bookChapter" .= _bookReferenceDTOBookChapter
      , "content" .= _bookReferenceDTOContent
      , "createdAt" .= _bookReferenceDTOCreatedAt
      , "updatedAt" .= _bookReferenceDTOUpdatedAt
      ]
