module Wizard.Api.Resource.TypeHint.TypeHintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Event

data TypeHintRequestDTO = TypeHintRequestDTO
  { packageId :: Maybe String
  , events :: [Event]
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)
