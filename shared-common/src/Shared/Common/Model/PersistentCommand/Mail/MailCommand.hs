module Shared.Common.Model.PersistentCommand.Mail.MailCommand where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data MailCommand = MailCommand
  { mode :: String
  , template :: String
  , recipients :: [MailRecipient]
  , parameters :: M.Map String Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON MailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MailCommand where
  toJSON = genericToJSON jsonOptions

data MailRecipient = MailRecipient
  { uuid :: Maybe U.UUID
  , email :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON MailRecipient where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MailRecipient where
  toJSON = genericToJSON jsonOptions
