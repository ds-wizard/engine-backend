module Shared.Common.Model.PersistentCommand.Mail.MailCommand where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data MailCommand = MailCommand
  { mode :: String
  , template :: String
  , recipients :: [String]
  , parameters :: M.Map String Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON MailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MailCommand where
  toJSON = genericToJSON jsonOptions

uuid :: U.UUID -> Value
uuid = String . U.toText

string :: String -> Value
string = String . T.pack

bool :: Bool -> Value
bool = Bool

maybeString :: Maybe String -> Value
maybeString = maybe Null (String . T.pack)

datetime :: UTCTime -> Value
datetime = toJSON
