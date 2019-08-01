module Api.Resource.Info.InfoDTO where

import GHC.Generics

data InfoDTO = InfoDTO
  { _infoDTOName :: String
  , _infoDTOVersion :: String
  , _infoDTOBuiltAt :: String
  } deriving (Show, Eq, Generic)
