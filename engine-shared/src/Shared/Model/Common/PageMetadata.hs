module Shared.Model.Common.PageMetadata where

import GHC.Generics

data PageMetadata =
  PageMetadata
    { _pageMetadataSize :: Int
    , _pageMetadataTotalElements :: Int
    , _pageMetadataTotalPages :: Int
    , _pageMetadataNumber :: Int
    }
  deriving (Show, Eq, Generic)
