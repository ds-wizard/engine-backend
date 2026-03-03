module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelPackageSimple = KnowledgeModelPackageSimple
  { uuid :: U.UUID
  , name :: String
  , version :: String
  }
  deriving (Generic, Eq, Show)
