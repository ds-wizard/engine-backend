module Wizard.Api.Resource.PersistentCommand.PersistentCommandChangeDTO where

import GHC.Generics

import Wizard.Model.PersistentCommand.PersistentCommand

data PersistentCommandChangeDTO = PersistentCommandChangeDTO
  { state :: PersistentCommandState
  }
  deriving (Show, Eq, Generic)
