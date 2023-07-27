module Shared.Common.Model.Dev.Dev where

import GHC.Generics

import Shared.Common.Api.Resource.Dev.DevExecutionDTO

data DevSection m = DevSection
  { name :: String
  , description :: Maybe String
  , operations :: [DevOperation m]
  }
  deriving (Generic)

data DevOperation m = DevOperation
  { name :: String
  , description :: Maybe String
  , parameters :: [DevOperationParameter]
  , function :: DevExecutionDTO -> m String
  }
  deriving (Generic)

data DevOperationParameter = DevOperationParameter
  { name :: String
  , aType :: DevOperationParameterType
  }
  deriving (Show, Eq, Generic)

data DevOperationParameterType
  = StringDevOperationParameterType
  | IntDevOperationParameterType
  | DoubleDevOperationParameterType
  | BoolDevOperationParameterType
  | JsonDevOperationParameterType
  deriving (Show, Eq, Generic)
