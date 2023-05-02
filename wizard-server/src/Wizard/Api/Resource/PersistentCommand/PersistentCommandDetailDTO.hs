module Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.PersistentCommand.PersistentCommand

data PersistentCommandDetailDTO = PersistentCommandDetailDTO
  { uuid :: U.UUID
  , state :: PersistentCommandState
  , component :: String
  , function :: String
  , body :: String
  , lastErrorMessage :: Maybe String
  , attempts :: Int
  , maxAttempts :: Int
  , app :: AppDTO
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
