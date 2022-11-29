module Shared.Model.Common.Sort where

data SortDirection
  = Ascending
  | Descending

data Sort = Sort
  { by :: String
  , direction :: SortDirection
  }
