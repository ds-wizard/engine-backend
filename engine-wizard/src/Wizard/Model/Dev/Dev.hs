module Wizard.Model.Dev.Dev where

import GHC.Generics

data DevSection =
  DevSection
    { _devSectionName :: String
    , _devSectionDescription :: Maybe String
    , _devSectionOperations :: [DevOperation]
    }
  deriving (Show, Eq, Generic)

data DevOperation =
  DevOperation
    { _devOperationName :: String
    , _devOperationDescription :: Maybe String
    , _devOperationParameters :: [DevOperationParameter]
    }
  deriving (Show, Eq, Generic)

data DevOperationParameter =
  DevOperationParameter
    { _devOperationParameterName :: String
    , _devOperationParameterAType :: DevOperationParameterType
    }
  deriving (Show, Eq, Generic)

data DevOperationParameterType
  = StringDevOperationParameterType
  | IntDevOperationParameterType
  | DoubleDevOperationParameterType
  | BoolDevOperationParameterType
  | JsonDevOperationParameterType
  deriving (Show, Eq, Generic)
