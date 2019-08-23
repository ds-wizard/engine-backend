module Api.Resource.Typehint.TypehintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventDTO

data TypehintRequestDTO = TypehintRequestDTO
  { _typehintRequestDTOPackageId :: Maybe String
  , _typehintRequestDTOEvents :: [EventDTO]
  , _typehintRequestDTOQuestionUuid :: U.UUID
  , _typehintRequestDTOQ :: String
  } deriving (Show, Eq, Generic)
