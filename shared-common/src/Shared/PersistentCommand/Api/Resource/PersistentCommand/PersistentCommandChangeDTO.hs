module Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeDTO where

import GHC.Generics

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

data PersistentCommandChangeDTO = PersistentCommandChangeDTO
  { state :: PersistentCommandState
  }
  deriving (Show, Eq, Generic)
