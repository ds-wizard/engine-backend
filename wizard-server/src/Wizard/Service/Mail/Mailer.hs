module Wizard.Service.Mail.Mailer where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import qualified Data.Vector as Vector

import Data.Aeson.Types
import Shared.Common.Model.Config.ServerConfig
import qualified Shared.Common.Model.PersistentCommand.Mail.MailCommand as MC
import Shared.Common.Util.JSON
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadNotificationJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadNotification
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Model.User.UserSuggestion
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import WizardLib.Public.Model.User.UserToken

sendRegistrationConfirmationMail :: User -> String -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash clientUrl =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "registrationConfirmation"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("hash", MC.string hash)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendRegistrationCreatedAnalyticsMail :: User -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "registrationCreatedAnalytics"
            , recipients = [serverConfig.analyticalMails.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "resetPassword"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("hash", MC.string hash)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendTwoFactorAuthMail :: UserDTO -> String -> AppContextM ()
sendTwoFactorAuthMail user code =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "twoFactorAuth"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("code", MC.string code)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendQuestionnaireInvitationMail :: Questionnaire -> Questionnaire -> AppContextM ()
sendQuestionnaireInvitationMail oldQtn newQtn =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail tenantConfig clientUrl currentUser) (filter filterPermissions newQtn.permissions)
  where
    filterPermissions :: QuestionnairePerm -> Bool
    filterPermissions perm = perm.memberUuid `notElem` fmap (.memberUuid) oldQtn.permissions
    sendOneEmail :: TenantConfig -> String -> UserDTO -> QuestionnairePerm -> AppContextM ()
    sendOneEmail tenantConfig clientUrl currentUser permission =
      case permission.memberType of
        UserGroupQuestionnairePermType -> return ()
        UserQuestionnairePermType -> do
          user <- findUserByUuid permission.memberUuid
          let body =
                MC.MailCommand
                  { mode = "wizard"
                  , template = "questionnaireInvitation"
                  , recipients = [user.email]
                  , parameters =
                      M.fromList
                        [ ("userUuid", MC.uuid user.uuid)
                        , ("clientUrl", MC.string clientUrl)
                        , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                        , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                        , ("inviteeUuid", MC.uuid user.uuid)
                        , ("inviteeFirstName", MC.string user.firstName)
                        , ("inviteeLastName", MC.string user.lastName)
                        , ("inviteeEmail", MC.string user.email)
                        , ("questionnaireUuid", MC.uuid newQtn.uuid)
                        , ("questionnaireName", MC.string newQtn.name)
                        , ("ownerUuid", MC.uuid currentUser.uuid)
                        , ("ownerFirstName", MC.string currentUser.firstName)
                        , ("ownerLastName", MC.string currentUser.lastName)
                        , ("ownerEmail", MC.string currentUser.email)
                        ]
                  }
          sendEmail body currentUser.uuid

sendQuestionnaireCommentThreadAssignedMail :: [QuestionnaireCommentThreadNotification] -> AppContextM ()
sendQuestionnaireCommentThreadAssignedMail notifications =
  runInTransaction $ do
    case notifications of
      [] -> return ()
      notification : _ -> do
        let notificationFn n =
              A.Object . KM.fromList $
                [ ("questionnaireUuid", MC.uuid n.questionnaireUuid)
                , ("questionnaireName", MC.string n.questionnaireName)
                , ("commentThreadUuid", MC.uuid n.commentThreadUuid)
                , ("path", MC.string n.path)
                , ("resolved", MC.bool n.resolved)
                , ("private", MC.bool n.private)
                , ("assignedBy", A.toJSON n.assignedBy)
                , ("text", MC.string n.text)
                ]
        let body =
              MC.MailCommand
                { mode = "wizard"
                , template = "commentThreadAssigned"
                , recipients = [notification.assignedTo.email]
                , parameters =
                    M.fromList
                      [ ("userFirstName", MC.string notification.assignedTo.firstName)
                      , ("notifications", A.Array . Vector.fromList . fmap notificationFn $ notifications)
                      , ("clientUrl", MC.string notification.clientUrl)
                      , ("appTitle", MC.maybeString notification.appTitle)
                      , ("supportEmail", MC.maybeString notification.supportEmail)
                      ]
                }
        sendEmailWithTenant body notification.assignedTo.uuid notification.tenantUuid

sendApiKeyCreatedMail :: UserDTO -> UserToken -> AppContextM ()
sendApiKeyCreatedMail user userToken =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "apiKeyCreated"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("tokenName", MC.string userToken.name)
                  , ("tokenExpiresAt", MC.datetime userToken.expiresAt)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendApiKeyExpirationMail :: UserDTO -> UserToken -> AppContextM ()
sendApiKeyExpirationMail user userToken =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "apiKeyExpiration"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("tokenName", MC.string userToken.name)
                  , ("tokenExpiresAt", MC.datetime userToken.expiresAt)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => dto -> U.UUID -> AppContextM ()
sendEmail dto createdBy = do
  tenantUuid <- asks currentTenantUuid
  sendEmailWithTenant dto createdBy tenantUuid

sendEmailWithTenant :: ToJSON dto => dto -> U.UUID -> U.UUID -> AppContextM ()
sendEmailWithTenant dto createdBy tenantUuid = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand uuid "mailer" "sendMail" body 10 False Nothing tenantUuid (Just . U.toString $ createdBy) now
    insertPersistentCommand command
    return ()
