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
import qualified Shared.Common.Util.Aeson as A
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
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
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
            , recipients = [MC.MailRecipient {uuid = Nothing, email = serverConfig.analyticalMails.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
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
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
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
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("code", A.string code)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
                  ]
            }
    sendEmail body user.uuid

sendQuestionnaireInvitationMail :: Questionnaire -> Questionnaire -> AppContextM ()
sendQuestionnaireInvitationMail oldQtn newQtn =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail tenantConfig clientUrl currentUser) (filter (filterPermissions currentUser) newQtn.permissions)
  where
    filterPermissions :: UserDTO -> QuestionnairePerm -> Bool
    filterPermissions currentUser perm = perm.memberUuid /= currentUser.uuid && perm.memberUuid `notElem` fmap (.memberUuid) oldQtn.permissions
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
                  , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
                  , parameters =
                      M.fromList
                        [ ("userUuid", A.uuid user.uuid)
                        , ("clientUrl", A.string clientUrl)
                        , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                        , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                        , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                        , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                        , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                        , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
                        , ("inviteeUuid", A.uuid user.uuid)
                        , ("inviteeFirstName", A.string user.firstName)
                        , ("inviteeLastName", A.string user.lastName)
                        , ("inviteeEmail", A.string user.email)
                        , ("questionnaireUuid", A.uuid newQtn.uuid)
                        , ("questionnaireName", A.string newQtn.name)
                        , ("ownerUuid", A.uuid currentUser.uuid)
                        , ("ownerFirstName", A.string currentUser.firstName)
                        , ("ownerLastName", A.string currentUser.lastName)
                        , ("ownerEmail", A.string currentUser.email)
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
                [ ("questionnaireUuid", A.uuid n.questionnaireUuid)
                , ("questionnaireName", A.string n.questionnaireName)
                , ("commentThreadUuid", A.uuid n.commentThreadUuid)
                , ("path", A.string n.path)
                , ("resolved", A.bool n.resolved)
                , ("private", A.bool n.private)
                , ("assignedBy", A.toJSON n.assignedBy)
                , ("text", A.string n.text)
                ]
        let body =
              MC.MailCommand
                { mode = "wizard"
                , template = "commentThreadAssigned"
                , recipients = [MC.MailRecipient {uuid = Just notification.assignedTo.uuid, email = notification.assignedTo.email}]
                , parameters =
                    M.fromList
                      [ ("userFirstName", A.string notification.assignedTo.firstName)
                      , ("notifications", A.Array . Vector.fromList . fmap notificationFn $ notifications)
                      , ("clientUrl", A.string notification.clientUrl)
                      , ("appTitle", A.maybeString notification.appTitle)
                      , ("logoUrl", A.maybeString notification.logoUrl)
                      , ("primaryColor", A.maybeString notification.primaryColor)
                      , ("illustrationsColor", A.maybeString notification.illustrationsColor)
                      , ("supportEmail", A.maybeString notification.supportEmail)
                      , ("mailConfigUuid", A.maybeUuid notification.mailConfigUuid)
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
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("tokenName", A.string userToken.name)
                  , ("tokenExpiresAt", A.datetime userToken.expiresAt)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
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
            , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
            , parameters =
                M.fromList
                  [ ("userUuid", A.uuid user.uuid)
                  , ("userFirstName", A.string user.firstName)
                  , ("userLastName", A.string user.lastName)
                  , ("userEmail", A.string user.email)
                  , ("tokenName", A.string userToken.name)
                  , ("tokenExpiresAt", A.datetime userToken.expiresAt)
                  , ("clientUrl", A.string clientUrl)
                  , ("appTitle", A.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tenantConfig.lookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tenantConfig.lookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tenantConfig.lookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tenantConfig.mailConfigUuid)
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
