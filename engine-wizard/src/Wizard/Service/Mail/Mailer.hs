module Wizard.Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  , sendQuestionnaireInvitationMail
  ) where

import Control.Lens ((^.), (^..))
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.JSON
import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Acl.Acl
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.PersistentCommand.Mail.SendQuestionnaireInvitationMailCommand
import Wizard.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand
import Wizard.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand
import Wizard.Model.PersistentCommand.Mail.SendResetPasswordMailCommand
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Service.App.AppHelper
import Wizard.Service.PersistentCommand.PersistentCommandMapper

sendRegistrationConfirmationMail :: UserDTO -> String -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash clientUrl =
  runInTransaction $ do
    let body =
          SendRegistrationConfirmationMailCommand
            { _sendRegistrationConfirmationMailCommandEmail = user ^. email
            , _sendRegistrationConfirmationMailCommandUserUuid = user ^. uuid
            , _sendRegistrationConfirmationMailCommandUserFirstName = user ^. firstName
            , _sendRegistrationConfirmationMailCommandUserLastName = user ^. lastName
            , _sendRegistrationConfirmationMailCommandUserEmail = user ^. email
            , _sendRegistrationConfirmationMailCommandHash = hash
            , _sendRegistrationConfirmationMailCommandClientUrl = clientUrl
            }
    sendEmail "sendRegistrationConfirmationMail" body (user ^. uuid)

sendRegistrationCreatedAnalyticsMail :: UserDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    clientUrl <- getAppClientUrl
    let body =
          SendRegistrationCreatedAnalyticsMailCommand
            { _sendRegistrationCreatedAnalyticsMailCommandEmail = serverConfig ^. analytics . email
            , _sendRegistrationCreatedAnalyticsMailCommandUserUuid = user ^. uuid
            , _sendRegistrationCreatedAnalyticsMailCommandUserFirstName = user ^. firstName
            , _sendRegistrationCreatedAnalyticsMailCommandUserLastName = user ^. lastName
            , _sendRegistrationCreatedAnalyticsMailCommandUserEmail = user ^. email
            , _sendRegistrationCreatedAnalyticsMailCommandClientUrl = clientUrl
            }
    sendEmail "sendRegistrationCreatedAnalyticsMail" body (user ^. uuid)

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    clientUrl <- getAppClientUrl
    let body =
          SendResetPasswordMailCommand
            { _sendResetPasswordMailCommandEmail = user ^. email
            , _sendResetPasswordMailCommandUserUuid = user ^. uuid
            , _sendResetPasswordMailCommandUserFirstName = user ^. firstName
            , _sendResetPasswordMailCommandUserLastName = user ^. lastName
            , _sendResetPasswordMailCommandUserEmail = user ^. email
            , _sendResetPasswordMailCommandHash = hash
            , _sendResetPasswordMailCommandClientUrl = clientUrl
            }
    sendEmail "sendResetPasswordMail" body (user ^. uuid)

sendQuestionnaireInvitationMail :: Questionnaire -> Questionnaire -> AppContextM ()
sendQuestionnaireInvitationMail oldQtn newQtn =
  runInTransaction $ do
    clientUrl <- getAppClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail clientUrl currentUser) (filter filterPermissions (newQtn ^. permissions))
  where
    filterPermissions :: QuestionnairePermRecord -> Bool
    filterPermissions perm = perm ^. member `notElem` (oldQtn ^.. permissions . traverse . member)
    sendOneEmail :: String -> UserDTO -> QuestionnairePermRecord -> AppContextM ()
    sendOneEmail clientUrl currentUser permission =
      case permission ^. member of
        GroupMember {..} -> return ()
        UserMember {_userMemberUuid = userUuid} -> do
          user <- findUserById (U.toString userUuid)
          let body =
                SendQuestionnaireInvitationMailCommand
                  { _sendQuestionnaireInvitationMailCommandEmail = user ^. email
                  , _sendQuestionnaireInvitationMailCommandClientUrl = clientUrl
                  , _sendQuestionnaireInvitationMailCommandInviteeUuid = user ^. uuid
                  , _sendQuestionnaireInvitationMailCommandInviteeFirstName = user ^. firstName
                  , _sendQuestionnaireInvitationMailCommandInviteeLastName = user ^. lastName
                  , _sendQuestionnaireInvitationMailCommandInviteeEmail = user ^. email
                  , _sendQuestionnaireInvitationMailCommandQuestionnaireUuid = newQtn ^. uuid
                  , _sendQuestionnaireInvitationMailCommandQuestionnaireName = newQtn ^. name
                  , _sendQuestionnaireInvitationMailCommandOwnerUuid = currentUser ^. uuid
                  , _sendQuestionnaireInvitationMailCommandOwnerFirstName = currentUser ^. firstName
                  , _sendQuestionnaireInvitationMailCommandOwnerLastName = currentUser ^. lastName
                  , _sendQuestionnaireInvitationMailCommandOwnerEmail = currentUser ^. email
                  }
          sendEmail "sendQuestionnaireInvitationMail" body (currentUser ^. uuid)

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => String -> dto -> U.UUID -> AppContextM ()
sendEmail function dto createdBy = do
  runInTransaction $ do
    appUuid <- asks _appContextAppUuid
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand pUuid "mailer" function body 10 False appUuid (Just createdBy) now
    insertPersistentCommand command
    return ()
