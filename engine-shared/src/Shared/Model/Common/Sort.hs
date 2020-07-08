module Shared.Model.Common.Sort where

data SortDirection
  = Ascending
  | Descending

data Sort =
  Sort
    { _sortBy :: String
    , _sortDirection :: SortDirection
    }
