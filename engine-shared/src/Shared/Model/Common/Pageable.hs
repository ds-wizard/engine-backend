module Shared.Model.Common.Pageable where

data Pageable = Pageable
  { page :: Maybe Int
  , size :: Maybe Int
  }
