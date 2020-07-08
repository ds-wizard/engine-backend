module Shared.Model.Common.Pageable where

data Pageable =
  Pageable
    { _pageablePage :: Maybe Int
    , _pageableSize :: Maybe Int
    }
