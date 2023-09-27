module Wizard.Model.PersistentCommand.Config.InvokeClientCssCompilationCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data InvokeClientCssCompilationCommand = InvokeClientCssCompilationCommand
  { tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON InvokeClientCssCompilationCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON InvokeClientCssCompilationCommand where
  toJSON = genericToJSON jsonOptions
