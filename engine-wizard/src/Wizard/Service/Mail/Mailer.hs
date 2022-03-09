module Wizard.Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  , sendQuestionnaireInvitationMail
  ) where

import Control.Lens ((^.), (^..))
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
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
            , _sendRegistrationConfirmationMailCommandHash = hash
            , _sendRegistrationConfirmationMailCommandClientUrl = clientUrl
            }
    sendEmail "sendRegistrationConfirmationMail" (BSL.unpack . encode $ body) (user ^. uuid)

sendRegistrationCreatedAnalyticsMail :: UserDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    clientUrl <- getAppClientUrl
    let body =
          SendRegistrationCreatedAnalyticsMailCommand
            { _sendRegistrationCreatedAnalyticsMailCommandEmail = serverConfig ^. analytics . email
            , _sendRegistrationCreatedAnalyticsMailCommandUserUuid = user ^. uuid
            , _sendRegistrationCreatedAnalyticsMailCommandClientUrl = clientUrl
            }
    sendEmail "sendRegistrationCreatedAnalyticsMail" (BSL.unpack . encode $ body) (user ^. uuid)

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    clientUrl <- getAppClientUrl
    let body =
          SendResetPasswordMailCommand
            { _sendResetPasswordMailCommandEmail = user ^. email
            , _sendResetPasswordMailCommandUserUuid = user ^. uuid
            , _sendResetPasswordMailCommandHash = hash
            , _sendResetPasswordMailCommandClientUrl = clientUrl
            }
    sendEmail "sendResetPasswordMail" (BSL.unpack . encode $ body) (user ^. uuid)

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
                  , _sendQuestionnaireInvitationMailCommandInviteeUuid = userUuid
                  , _sendQuestionnaireInvitationMailCommandQuestionnaireUuid = newQtn ^. uuid
                  , _sendQuestionnaireInvitationMailCommandOwnerUuid = currentUser ^. uuid
                  }
          sendEmail "sendQuestionnaireInvitationMail" (BSL.unpack . encode $ body) (currentUser ^. uuid)

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: String -> String -> U.UUID -> AppContextM ()
sendEmail function body createdBy = do
  runInTransaction $ do
    appUuid <- asks _appContextAppUuid
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let command = toPersistentCommand pUuid "Mailer" function body 10 False appUuid createdBy now
    insertPersistentCommand command
    return ()
