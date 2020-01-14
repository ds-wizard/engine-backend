module Wizard.Database.BSON.BookReference.BookReference where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Wizard.Model.BookReference.BookReference

instance ToBSON BookReference where
  toBSON br =
    [ "shortUuid" BSON.=: (br ^. shortUuid)
    , "bookChapter" BSON.=: (br ^. bookChapter)
    , "content" BSON.=: (br ^. content)
    , "createdAt" BSON.=: (br ^. createdAt)
    , "updatedAt" BSON.=: (br ^. updatedAt)
    ]

instance FromBSON BookReference where
  fromBSON doc = do
    shortUuid <- BSON.lookup "shortUuid" doc
    bookChapter <- BSON.lookup "bookChapter" doc
    content <- BSON.lookup "content" doc
    createdAt <- BSON.lookup "createdAt" doc
    updatedAt <- BSON.lookup "updatedAt" doc
    return
      BookReference
        { _bookReferenceShortUuid = shortUuid
        , _bookReferenceBookChapter = bookChapter
        , _bookReferenceContent = content
        , _bookReferenceCreatedAt = createdAt
        , _bookReferenceUpdatedAt = updatedAt
        }
