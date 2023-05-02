module Shared.Common.Model.Common.PageMetadata where

import GHC.Generics

data PageMetadata = PageMetadata
  { size :: Int
  , totalElements :: Int
  , totalPages :: Int
  , number :: Int
  }
  deriving (Show, Eq, Generic)
