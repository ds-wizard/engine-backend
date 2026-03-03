module Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelPackageSuggestion = KnowledgeModelPackageSuggestion
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , description :: String
  }
  deriving (Show, Eq, Ord, Generic)
