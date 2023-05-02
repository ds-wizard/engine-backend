module Wizard.Api.Resource.Typehint.TypehintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Event

data TypehintRequestDTO = TypehintRequestDTO
  { packageId :: Maybe String
  , events :: [Event]
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)
