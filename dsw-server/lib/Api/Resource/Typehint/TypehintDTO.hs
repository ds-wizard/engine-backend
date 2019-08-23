module Api.Resource.Typehint.TypehintDTO where

import GHC.Generics

data TypehintDTO = TypehintDTO
  { _typehintDTOIntId :: String
  , _typehintDTOName :: String
  , _typehintDTOUrl :: String
  } deriving (Show, Eq, Generic)
