module Wizard.Service.Mail.Mailer (
  sendRegistrationConfirmationMail,
  sendRegistrationCreatedAnalyticsMail,
  sendResetPasswordMail,
  sendQuestionnaireInvitationMail,
  sendTwoFactorAuthMail,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.JSON
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Acl.Acl
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.PersistentCommand.Mail.SendQuestionnaireInvitationMailCommand
import Wizard.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand
import Wizard.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand
import Wizard.Model.PersistentCommand.Mail.SendResetPasswordMailCommand
import Wizard.Model.PersistentCommand.Mail.SendTwoFactorAuthMailCommand
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.User.User
import Wizard.Service.App.AppHelper

sendRegistrationConfirmationMail :: UserDTO -> String -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash clientUrl =
  runInTransaction $ do
    let body =
          SendRegistrationConfirmationMailCommand
            { email = user.email
            , userUuid = user.uuid
            , userFirstName = user.firstName
            , userLastName = user.lastName
            , userEmail = user.email
            , hash = hash
            , clientUrl = clientUrl
            }
    sendEmail "sendRegistrationConfirmationMail" body user.uuid

sendRegistrationCreatedAnalyticsMail :: UserDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    clientUrl <- getAppClientUrl
    let body =
          SendRegistrationCreatedAnalyticsMailCommand
            { email = serverConfig.analytics.email
            , userUuid = user.uuid
            , userFirstName = user.firstName
            , userLastName = user.lastName
            , userEmail = user.email
            , clientUrl = clientUrl
            }
    sendEmail "sendRegistrationCreatedAnalyticsMail" body user.uuid

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    clientUrl <- getAppClientUrl
    let body =
          SendResetPasswordMailCommand
            { email = user.email
            , userUuid = user.uuid
            , userFirstName = user.firstName
            , userLastName = user.lastName
            , userEmail = user.email
            , hash = hash
            , clientUrl = clientUrl
            }
    sendEmail "sendResetPasswordMail" body user.uuid

sendTwoFactorAuthMail :: UserDTO -> String -> AppContextM ()
sendTwoFactorAuthMail user code =
  runInTransaction $ do
    clientUrl <- getAppClientUrl
    let body =
          SendTwoFactorAuthMailCommand
            { email = user.email
            , userUuid = user.uuid
            , userFirstName = user.firstName
            , userLastName = user.lastName
            , userEmail = user.email
            , code = code
            , clientUrl = clientUrl
            }
    sendEmail "sendTwoFactorAuthMail" body user.uuid

sendQuestionnaireInvitationMail :: Questionnaire -> Questionnaire -> AppContextM ()
sendQuestionnaireInvitationMail oldQtn newQtn =
  runInTransaction $ do
    clientUrl <- getAppClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail clientUrl currentUser) (filter filterPermissions newQtn.permissions)
  where
    filterPermissions :: QuestionnairePermRecord -> Bool
    filterPermissions perm = perm.member `notElem` fmap (.member) oldQtn.permissions
    sendOneEmail :: String -> UserDTO -> QuestionnairePermRecord -> AppContextM ()
    sendOneEmail clientUrl currentUser permission =
      case permission.member of
        GroupMember {..} -> return ()
        UserMember {uuid = userUuid} -> do
          user <- findUserByUuid userUuid
          let body =
                SendQuestionnaireInvitationMailCommand
                  { email = user.email
                  , clientUrl = clientUrl
                  , inviteeUuid = user.uuid
                  , inviteeFirstName = user.firstName
                  , inviteeLastName = user.lastName
                  , inviteeEmail = user.email
                  , questionnaireUuid = newQtn.uuid
                  , questionnaireName = newQtn.name
                  , ownerUuid = currentUser.uuid
                  , ownerFirstName = currentUser.firstName
                  , ownerLastName = currentUser.lastName
                  , ownerEmail = currentUser.email
                  }
          sendEmail "sendQuestionnaireInvitationMail" body currentUser.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => String -> dto -> U.UUID -> AppContextM ()
sendEmail function dto createdBy = do
  runInTransaction $ do
    appUuid <- asks currentAppUuid
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand pUuid "mailer" function body 10 False Nothing appUuid (Just . U.toString $ createdBy) now
    insertPersistentCommand command
    return ()
