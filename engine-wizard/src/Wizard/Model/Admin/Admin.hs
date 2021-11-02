module Wizard.Model.Admin.Admin where

import GHC.Generics

data AdminSection =
  AdminSection
    { _adminSectionName :: String
    , _adminSectionDescription :: Maybe String
    , _adminSectionOperations :: [AdminOperation]
    }
  deriving (Show, Eq, Generic)

data AdminOperation =
  AdminOperation
    { _adminOperationName :: String
    , _adminOperationDescription :: Maybe String
    , _adminOperationParameters :: [AdminOperationParameter]
    }
  deriving (Show, Eq, Generic)

data AdminOperationParameter =
  AdminOperationParameter
    { _adminOperationParameterName :: String
    , _adminOperationParameterAType :: AdminOperationParameterType
    }
  deriving (Show, Eq, Generic)

data AdminOperationParameterType
  = StringAdminOperationParameterType
  | IntAdminOperationParameterType
  | DoubleAdminOperationParameterType
  | BoolAdminOperationParameterType
  | JsonAdminOperationParameterType
  deriving (Show, Eq, Generic)
