module Wizard.Model.PersistentCommand.Mail.SendQuestionnaireInvitationMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.JSON

data SendQuestionnaireInvitationMailCommand =
  SendQuestionnaireInvitationMailCommand
    { _sendQuestionnaireInvitationMailCommandEmail :: String
    , _sendQuestionnaireInvitationMailCommandClientUrl :: String
    , _sendQuestionnaireInvitationMailCommandInviteeUuid :: U.UUID
    , _sendQuestionnaireInvitationMailCommandQuestionnaireUuid :: U.UUID
    , _sendQuestionnaireInvitationMailCommandOwnerUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendQuestionnaireInvitationMailCommand where
  parseJSON = simpleParseJSON "_sendQuestionnaireInvitationMailCommand"

instance ToJSON SendQuestionnaireInvitationMailCommand where
  toJSON = simpleToJSON "_sendQuestionnaireInvitationMailCommand"
