module Wizard.Model.PersistentCommand.Mail.SendQuestionnaireInvitationMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data SendQuestionnaireInvitationMailCommand = SendQuestionnaireInvitationMailCommand
  { email :: String
  , clientUrl :: String
  , inviteeUuid :: U.UUID
  , inviteeFirstName :: String
  , inviteeLastName :: String
  , inviteeEmail :: String
  , questionnaireUuid :: U.UUID
  , questionnaireName :: String
  , ownerUuid :: U.UUID
  , ownerFirstName :: String
  , ownerLastName :: String
  , ownerEmail :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendQuestionnaireInvitationMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendQuestionnaireInvitationMailCommand where
  toJSON = genericToJSON jsonOptions
