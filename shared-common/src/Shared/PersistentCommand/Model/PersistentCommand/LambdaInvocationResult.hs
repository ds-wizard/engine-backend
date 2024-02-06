module Shared.PersistentCommand.Model.PersistentCommand.LambdaInvocationResult where

import Data.Aeson
import GHC.Generics

data LambdaInvocationResult = LambdaInvocationResult
  { statusCode :: Int
  , payload :: Maybe Value
  , executedVersion :: Maybe String
  , logResult :: Maybe String
  }
  deriving (Show, Generic)
