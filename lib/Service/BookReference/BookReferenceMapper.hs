module Service.BookReference.BookReferenceMapper where

import Control.Lens ((^.))

import Api.Resource.BookReference.BookReferenceDTO
import LensesConfig
import Model.BookReference.BookReference

toDTO :: BookReference -> BookReferenceDTO
toDTO br =
  BookReferenceDTO
  { _bookReferenceDTOShortUuid = br ^. shortUuid
  , _bookReferenceDTOContent = br ^. content
  , _bookReferenceDTOCreatedAt = br ^. createdAt
  , _bookReferenceDTOUpdatedAt = br ^. updatedAt
  }

fromDTO :: BookReferenceDTO -> BookReference
fromDTO dto =
  BookReference
  { _bookReferenceShortUuid = dto ^. shortUuid
  , _bookReferenceContent = dto ^. content
  , _bookReferenceCreatedAt = dto ^. createdAt
  , _bookReferenceUpdatedAt = dto ^. updatedAt
  }
