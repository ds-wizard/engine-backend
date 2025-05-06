module Registry.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

data ClientConfigDTO = ClientConfigDTO
  { authentication :: ClientConfigAuthDTO
  , locale :: ClientConfigLocaleDTO
  }
  deriving (Show, Eq, Generic)

data ClientConfigAuthDTO = ClientConfigAuthDTO
  { publicRegistrationEnabled :: Bool
  }
  deriving (Generic, Eq, Show)

data ClientConfigLocaleDTO = ClientConfigLocaleDTO
  { enabled :: Bool
  }
  deriving (Generic, Eq, Show)
