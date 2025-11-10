module Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion where

import GHC.Generics

data KnowledgeModelPackageSuggestion = KnowledgeModelPackageSuggestion
  { pId :: String
  , name :: String
  , version :: String
  , description :: String
  }
  deriving (Show, Eq, Ord, Generic)
