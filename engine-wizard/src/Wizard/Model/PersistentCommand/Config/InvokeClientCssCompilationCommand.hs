module Wizard.Model.PersistentCommand.Config.InvokeClientCssCompilationCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.JSON

data InvokeClientCssCompilationCommand =
  InvokeClientCssCompilationCommand
    { _invokeClientCssCompilationCommandAppUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

instance FromJSON InvokeClientCssCompilationCommand where
  parseJSON = simpleParseJSON "_invokeClientCssCompilationCommand"

instance ToJSON InvokeClientCssCompilationCommand where
  toJSON = simpleToJSON "_invokeClientCssCompilationCommand"
