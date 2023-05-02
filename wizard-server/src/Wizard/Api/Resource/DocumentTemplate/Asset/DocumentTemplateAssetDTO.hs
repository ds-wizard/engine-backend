module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data DocumentTemplateAssetDTO = DocumentTemplateAssetDTO
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , url :: String
  , urlExpiration :: UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
