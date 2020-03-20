module Shared.Api.Resource.Event.EventFieldDTO where

import GHC.Generics

data EventFieldDTO a
  = NothingChangedDTO
  | ChangedValueDTO a
  deriving (Show, Eq, Generic)

instance Functor EventFieldDTO where
  fmap f (ChangedValueDTO a) = ChangedValueDTO (f a)
  fmap _ NothingChangedDTO = NothingChangedDTO
