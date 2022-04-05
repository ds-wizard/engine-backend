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
    , _sendQuestionnaireInvitationMailCommandInviteeFirstName :: String
    , _sendQuestionnaireInvitationMailCommandInviteeLastName :: String
    , _sendQuestionnaireInvitationMailCommandInviteeEmail :: String
    , _sendQuestionnaireInvitationMailCommandQuestionnaireUuid :: U.UUID
    , _sendQuestionnaireInvitationMailCommandQuestionnaireName :: String
    , _sendQuestionnaireInvitationMailCommandOwnerUuid :: U.UUID
    , _sendQuestionnaireInvitationMailCommandOwnerFirstName :: String
    , _sendQuestionnaireInvitationMailCommandOwnerLastName :: String
    , _sendQuestionnaireInvitationMailCommandOwnerEmail :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendQuestionnaireInvitationMailCommand where
  parseJSON = simpleParseJSON "_sendQuestionnaireInvitationMailCommand"

instance ToJSON SendQuestionnaireInvitationMailCommand where
  toJSON = simpleToJSON "_sendQuestionnaireInvitationMailCommand"
