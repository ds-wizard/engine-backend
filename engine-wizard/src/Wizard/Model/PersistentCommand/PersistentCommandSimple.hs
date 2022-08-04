module Wizard.Model.PersistentCommand.PersistentCommandSimple where

import qualified Data.UUID as U
import GHC.Generics

data PersistentCommandSimple =
  PersistentCommandSimple
    { _persistentCommandSimpleUuid :: U.UUID
    , _persistentCommandSimpleAppUuid :: U.UUID
    , _persistentCommandSimpleCreatedBy :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
