module Api.Resource.Event.EventFieldDTO where

data EventFieldDTO a
  = NothingChangedDTO
  | ChangedValueDTO a
  deriving (Show, Eq)

instance Functor EventFieldDTO where
  fmap f (ChangedValueDTO a) = ChangedValueDTO (f a)
  fmap _ NothingChangedDTO = NothingChangedDTO
