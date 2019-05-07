module Api.Resource.Typehint.TypehintDTO where

data TypehintDTO = TypehintDTO
  { _typehintDTOIntId :: String
  , _typehintDTOName :: String
  , _typehintDTOUrl :: String
  } deriving (Show, Eq)
