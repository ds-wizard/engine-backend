module Wizard.Api.Resource.Typehint.TypehintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data TypehintRequestDTO =
  TypehintRequestDTO
    { _typehintRequestDTOPackageId :: Maybe String
    , _typehintRequestDTOEvents :: [Event]
    , _typehintRequestDTOQuestionUuid :: U.UUID
    , _typehintRequestDTOQ :: String
    }
  deriving (Show, Eq, Generic)
