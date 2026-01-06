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
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadNotificationJM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Comment.ProjectCommentThreadNotification
import Wizard.Model.Project.Project
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.Tenant.TenantHelper
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigMailDAO
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.User.UserSimple
import WizardLib.Public.Model.User.UserToken

sendRegistrationConfirmationMail :: User -> String -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash clientUrl =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
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
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendRegistrationCreatedAnalyticsMail :: User -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
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
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
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
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  ]
            }
    sendEmail body user.uuid

sendTwoFactorAuthMail :: UserDTO -> String -> AppContextM ()
sendTwoFactorAuthMail user code =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
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
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  ]
            }
    sendEmail body user.uuid

sendProjectInvitationMail :: Project -> Project -> AppContextM ()
sendProjectInvitationMail oldProject newProject =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
    clientUrl <- getClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail tcPrivacyAndSupport tcLookAndFeel tcMail clientUrl currentUser) (filter (filterPermissions currentUser) newProject.permissions)
  where
    filterPermissions :: UserDTO -> ProjectPerm -> Bool
    filterPermissions currentUser perm = perm.memberUuid /= currentUser.uuid && perm.memberUuid `notElem` fmap (.memberUuid) oldProject.permissions
    sendOneEmail :: TenantConfigPrivacyAndSupport -> TenantConfigLookAndFeel -> TenantConfigMail -> String -> UserDTO -> ProjectPerm -> AppContextM ()
    sendOneEmail tcPrivacyAndSupport tcLookAndFeel tcMail clientUrl currentUser permission =
      case permission.memberType of
        UserGroupProjectPermType -> return ()
        UserProjectPermType -> do
          user <- findUserByUuid permission.memberUuid
          let body =
                MC.MailCommand
                  { mode = "wizard"
                  , template = "projectInvitation"
                  , recipients = [MC.MailRecipient {uuid = Just user.uuid, email = user.email}]
                  , parameters =
                      M.fromList
                        [ ("userUuid", A.uuid user.uuid)
                        , ("clientUrl", A.string clientUrl)
                        , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                        , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                        , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                        , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                        , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                        , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                        , ("inviteeUuid", A.uuid user.uuid)
                        , ("inviteeFirstName", A.string user.firstName)
                        , ("inviteeLastName", A.string user.lastName)
                        , ("inviteeEmail", A.string user.email)
                        , ("projectUuid", A.uuid newProject.uuid)
                        , ("projectName", A.string newProject.name)
                        , ("ownerUuid", A.uuid currentUser.uuid)
                        , ("ownerFirstName", A.string currentUser.firstName)
                        , ("ownerLastName", A.string currentUser.lastName)
                        , ("ownerEmail", A.string currentUser.email)
                        ]
                  }
          sendEmail body currentUser.uuid

sendProjectCommentThreadAssignedMail :: [ProjectCommentThreadNotification] -> AppContextM ()
sendProjectCommentThreadAssignedMail notifications =
  runInTransaction $ do
    case notifications of
      [] -> return ()
      notification : _ -> do
        let notificationFn n =
              A.Object . KM.fromList $
                [ ("projectUuid", A.uuid n.projectUuid)
                , ("projectName", A.string n.projectName)
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
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
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
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  ]
            }
    sendEmail body user.uuid

sendApiKeyExpirationMail :: User -> UserToken -> AppContextM ()
sendApiKeyExpirationMail user userToken =
  runInTransaction $ do
    tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
    tcLookAndFeel <- findTenantConfigLookAndFeel
    tcMail <- findTenantConfigMail
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
                  , ("appTitle", A.maybeString tcLookAndFeel.appTitle)
                  , ("logoUrl", A.maybeString tcLookAndFeel.logoUrl)
                  , ("primaryColor", A.maybeString tcLookAndFeel.primaryColor)
                  , ("illustrationsColor", A.maybeString tcLookAndFeel.illustrationsColor)
                  , ("supportEmail", A.maybeString tcPrivacyAndSupport.supportEmail)
                  , ("mailConfigUuid", A.maybeUuid tcMail.configUuid)
                  ]
            }
    sendEmailWithTenant body user.uuid user.tenantUuid

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
