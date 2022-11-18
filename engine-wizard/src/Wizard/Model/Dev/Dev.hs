module Wizard.Model.Dev.Dev where

import GHC.Generics

data DevSection = DevSection
  { name :: String
  , description :: Maybe String
  , operations :: [DevOperation]
  }
  deriving (Show, Eq, Generic)

data DevOperation = DevOperation
  { name :: String
  , description :: Maybe String
  , parameters :: [DevOperationParameter]
  }
  deriving (Show, Eq, Generic)

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
