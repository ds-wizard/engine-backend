module Shared.Model.Common.MapEntry where

import qualified Data.Map.Strict as M
import GHC.Generics

data MapEntry key value = MapEntry
  { key :: key
  , value :: value
  }
  deriving (Show, Eq, Generic)

mapEntryToMap :: Ord key => [MapEntry key value] -> M.Map key value
mapEntryToMap = M.fromList . fmap (\(MapEntry key value) -> (key, value))
